package mill.exec

import mill.api.{BuildCtx, PathRef, Task}
import mill.constants.Util
import ujson.{Arr, Null, Num, Obj, Str, Value}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import java.util.UUID
import scala.collection.mutable
import scala.util.control.NonFatal

/**
 * Opt-in JSONL diagnostics for explaining remote-cache keys.
 *
 * The trace is deliberately best-effort: diagnostics must never change whether a build succeeds.
 * Set `MILL_REMOTE_CACHE_TRACE_FILE` to enable it. Paths are rendered relative to the workspace or
 * home directory; paths outside both roots are represented by an opaque digest.
 */
private[mill] final class RemoteCacheTrace private (
    file: os.Path,
    workspace: os.Path,
    runId: String,
    commit: Option[String]
) {
  private val lock = new Object()
  private val fileDigests = mutable.Map.empty[String, String]

  def recordTaskKey(
      task: String,
      inputsHash: Int,
      externalInputsHash: Int,
      sideHashes: Int,
      scriptsHash: Int,
      externalInputs: Seq[(String, Int)],
      sideHashEntries: Seq[(String, Int)],
      codeSignatures: Seq[(String, Int)],
      actionKey: String,
      salt: Option[String]
  ): Unit = write(Obj(
    "type" -> "task-key",
    "task" -> task,
    "inputsHash" -> inputsHash,
    "externalInputsHash" -> externalInputsHash,
    "sideHashes" -> sideHashes,
    "scriptsHash" -> scriptsHash,
    "externalInputs" -> pairs(externalInputs),
    "sideHashEntries" -> pairs(sideHashEntries),
    "codeSignatures" -> pairs(codeSignatures),
    "actionKey" -> actionKey,
    "salt" -> salt.fold[Value](Null)(Str(_))
  ))

  def recordValue(
      task: Task[?],
      valueHash: Int,
      hashMode: String,
      json: Option[Value],
      pathRefs: Seq[PathRef]
  ): Unit = write(Obj(
    "type" -> "value-hash",
    "task" -> taskName(task),
    "valueHash" -> valueHash,
    "hashMode" -> hashMode,
    "jsonSha256" -> json.fold[Value](Null)(value => Str(sha256(value.render()))),
    "pathRefs" -> Arr.from(pathRefs.map(pathRefJson))
  ))

  def requestHeaders(task: String, inputsHash: Int, actionKey: String): Seq[(String, String)] =
    Seq(
      "X-Mill-Cache-Trace-Run" -> headerValue(runId),
      "X-Mill-Cache-Task" -> headerValue(task),
      "X-Mill-Cache-Inputs-Hash" -> inputsHash.toString,
      "X-Mill-Cache-Action-Key" -> actionKey
    ) ++ commit.toSeq.map(value => "X-Mill-Cache-Commit" -> headerValue(value))

  def recordRequest(
      task: String,
      operation: String,
      key: String,
      actionKey: String,
      outcome: String
  ): Unit = write(Obj(
    "type" -> "cache-request",
    "task" -> task,
    "operation" -> operation,
    "key" -> key,
    "actionKey" -> actionKey,
    "outcome" -> outcome
  ))

  private def pairs(values: Seq[(String, Int)]): Arr =
    Arr.from(values.map { case (name, hash) => Obj("name" -> name, "hash" -> hash) })

  private def taskName(task: Task[?]): String = task match {
    case named: Task.Named[?] => named.ctx.segments.render
    case _ => task.getClass.getName
  }

  private def pathRefJson(pathRef: PathRef): Obj = {
    val path = pathRef.path
    val realPath = PathRef.toAbsString(path)
    val exists = os.exists(path, followLinks = false)
    val fileType =
      if (!exists) "missing"
      else os.stat(path, followLinks = false).fileType.toString
    val size =
      if (exists && os.isFile(path, followLinks = false)) Some(os.size(path))
      else None
    val mtime = if (exists) Some(os.mtime(path)) else None
    val permissions = if (exists) Some(os.perms(path).toInt()) else None
    val contentSha256 =
      if (exists && os.isFile(path, followLinks = false)) Some(fileSha256(path, realPath))
      else None

    Obj(
      "path" -> displayPath(path, realPath),
      "quick" -> pathRef.quick,
      "sig" -> pathRef.sig,
      "revalidate" -> pathRef.revalidate.toString,
      "fileType" -> fileType,
      "size" -> size.fold[Value](Null)(Num(_)),
      "mtime" -> mtime.fold[Value](Null)(Num(_)),
      "permissions" -> permissions.fold[Value](Null)(Num(_)),
      "contentSha256" -> contentSha256.fold[Value](Null)(Str(_))
    )
  }

  private def displayPath(path: os.Path, realPath: String): String = {
    val resolved = PathRef.toResolvedOsPath(path)
    val resolvedWorkspace = PathRef.toResolvedOsPath(workspace)
    val resolvedHome = PathRef.toResolvedOsPath(os.home)
    if (resolved.startsWith(resolvedWorkspace)) {
      val relative = resolved.relativeTo(resolvedWorkspace)
      if (relative.segments.isEmpty) "workspace" else s"workspace/$relative"
    } else if (resolved.startsWith(resolvedHome)) {
      val relative = resolved.relativeTo(resolvedHome)
      if (relative.segments.isEmpty) "home" else s"home/$relative"
    } else {
      val name = Option(path.last).filter(_.nonEmpty).getOrElse("path")
      s"external/${sha256(realPath).take(16)}/$name"
    }
  }

  private def fileSha256(path: os.Path, realPath: String): String = lock.synchronized {
    fileDigests.getOrElseUpdate(realPath, {
      val digest = MessageDigest.getInstance("SHA-256")
      val input = os.read.inputStream(path)
      val buffer = new Array[Byte](64 * 1024)
      try {
        var read = input.read(buffer)
        while (read != -1) {
          digest.update(buffer, 0, read)
          read = input.read(buffer)
        }
      } finally input.close()
      Util.hexArray(digest.digest())
    })
  }

  private def sha256(value: String): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(value.getBytes(StandardCharsets.UTF_8))
    Util.hexArray(digest.digest())
  }

  private def headerValue(value: String): String =
    value.iterator.filter(ch => ch >= ' ' && ch != '\u007f').take(512).mkString

  private def write(fields: Obj): Unit = {
    try BuildCtx.withFilesystemCheckerDisabled {
      RemoteCacheTrace.writeLock.synchronized {
        val event = Obj(
          "schemaVersion" -> 1,
          "runId" -> runId,
          "commit" -> commit.fold[Value](Null)(Str(_)),
          "time" -> Instant.now().toString
        )
        fields.value.foreach { case (key, value) => event(key) = value }
        os.write.append(file, event.render() + "\n", createFolders = true)
      }
    }
    catch {
      case NonFatal(_) =>
    }
  }
}

private[mill] object RemoteCacheTrace {
  private val TraceFile = "MILL_REMOTE_CACHE_TRACE_FILE"
  private val TraceRunId = "MILL_REMOTE_CACHE_TRACE_RUN_ID"
  private val TraceCommit = "MILL_REMOTE_CACHE_TRACE_COMMIT"
  private val writeLock = new Object()

  def fromEnv(env: Map[String, String], workspace: os.Path): Option[RemoteCacheTrace] =
    env.get(TraceFile).filter(_.nonEmpty).map { fileName =>
      val file = os.Path(fileName, workspace)
      val runId = env.get(TraceRunId).filter(_.nonEmpty).getOrElse(UUID.randomUUID().toString)
      new RemoteCacheTrace(file, workspace, runId, env.get(TraceCommit).filter(_.nonEmpty))
    }
}
