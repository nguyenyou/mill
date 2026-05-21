package mill.kotlinlib.worker.impl

import mill.api.TaskCtx
import org.jetbrains.kotlin.cli.common.messages.{
  MessageRenderer,
  OutputMessageUtil
}
import org.jetbrains.kotlin.cli.jvm.K2JVMCompiler
import org.w3c.dom.Element

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  FileInputStream,
  FileOutputStream,
  PrintStream
}
import java.nio.charset.StandardCharsets
import java.util.Locale
import java.util.Properties
import javax.xml.parsers.DocumentBuilderFactory
import scala.collection.mutable
import scala.util.Try

class JvmCompileImpl() extends Compiler {

  private def destinationDirectoryFromArgs(args: Seq[String])(using ctx: TaskCtx): os.Path = {
    args.sliding(2)
      .collectFirst {
        case Seq("-d", dir) => os.Path(dir, ctx.workspace)
      }
      .getOrElse(ctx.dest / "classes")
  }

  private def trackedOutputsFile(using ctx: TaskCtx): os.Path =
    ctx.dest / "kotlin-cli-output-files.properties"

  private def isInDirectory(path: os.Path, dir: os.Path): Boolean =
    path.toNIO.normalize().startsWith(dir.toNIO.normalize())

  private def loadTrackedOutputs(file: os.Path)(using ctx: TaskCtx): Option[Seq[os.Path]] = {
    if (!os.exists(file)) None
    else {
      Try {
        val props = new Properties()
        val input = new FileInputStream(file.toIO)
        try {
          props.load(input)
        } finally {
          input.close()
        }

        val count = Try(props.getProperty("count").toInt).getOrElse(0)
        (0 until count).flatMap { i =>
          Option(props.getProperty(s"output.$i")).flatMap { path =>
            Try(os.Path(path, ctx.workspace)).toOption
          }
        }
      }.toOption
    }
  }

  private def storeTrackedOutputs(file: os.Path, outputs: Seq[os.Path]): Unit = {
    os.makeDir.all(file / os.up)

    val props = new Properties()
    val distinctOutputs = outputs.distinct.sortBy(_.toString)
    props.setProperty("count", distinctOutputs.size.toString)
    distinctOutputs.zipWithIndex.foreach { case (output, index) =>
      props.setProperty(s"output.$index", output.toString)
    }

    val out = new FileOutputStream(file.toIO)
    try {
      props.store(out, "Kotlin CLI output files")
    } finally {
      out.close()
    }
  }

  private def removePreviousOutputs(destinationDirectory: os.Path)(using ctx: TaskCtx): Unit = {
    loadTrackedOutputs(trackedOutputsFile) match {
      case Some(outputs) =>
        outputs.filter(isInDirectory(_, destinationDirectory)).foreach(os.remove.all(_))
      case None =>
        os.remove.all(destinationDirectory)
        os.makeDir.all(destinationDirectory)
    }
  }

  private def xmlDocumentBuilderFactory: DocumentBuilderFactory = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
    factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    factory
  }

  private def renderCompilerMessage(element: Element)(using ctx: TaskCtx): Unit = {
    val tag = element.getTagName.toLowerCase(Locale.ROOT)
    val path = Option(element.getAttribute("path")).filter(_.nonEmpty)
    val line = Option(element.getAttribute("line")).filter(_.nonEmpty)
    val column = Option(element.getAttribute("column")).filter(_.nonEmpty)

    val location = path match {
      case Some(value) =>
        val lineColumn = line.map(l => column.map(c => s":$l:$c").getOrElse(s":$l")).getOrElse("")
        s"$value$lineColumn: "
      case None => ""
    }

    ctx.log.streams.err.println(s"$location$tag: ${element.getTextContent}")
  }

  private def parseCompilerOutput(
      output: ByteArrayOutputStream,
      destinationDirectory: os.Path
  )(using ctx: TaskCtx): Seq[os.Path] = {
    val outputText = output.toString(StandardCharsets.UTF_8)
    if (outputText.trim.isEmpty) Seq.empty
    else {
      Try {
        val document = xmlDocumentBuilderFactory.newDocumentBuilder().parse(
          ByteArrayInputStream(outputText.getBytes(StandardCharsets.UTF_8))
        )
        val outputFiles = mutable.LinkedHashSet.empty[os.Path]
        val messages = document.getDocumentElement.getChildNodes
        for (i <- 0 until messages.getLength) {
          messages.item(i) match {
            case element: Element if element.getTagName.equalsIgnoreCase("output") =>
              val outputFile = Option(OutputMessageUtil.parseOutputMessage(element.getTextContent))
                .flatMap(output => Option(output.outputFile))
                .map(file => os.Path(file.toPath))
                .filter(isInDirectory(_, destinationDirectory))

              outputFile.foreach(outputFiles.add)

            case element: Element =>
              renderCompilerMessage(element)

            case _ =>
          }
        }
        outputFiles.toSeq
      }.getOrElse {
        ctx.log.streams.err.print(outputText)
        Seq.empty
      }
    }
  }

  def compile(
      args: Seq[String],
      sources: Seq[os.Path]
  )(using
      ctx: TaskCtx
  ): (Int, String) = {

    val shouldTrackOutputs = args.contains("-Xreport-output-files")
    val allArgs = args ++ sources.map(_.toString)

    val compiler = K2JVMCompiler()
    val exitCode =
      if (shouldTrackOutputs) {
        val destinationDirectory = destinationDirectoryFromArgs(args)
        removePreviousOutputs(destinationDirectory)

        val output = ByteArrayOutputStream()
        val printer = PrintStream(output, true, StandardCharsets.UTF_8)
        val result = try {
          compiler.exec(printer, MessageRenderer.XML, allArgs*)
        } finally {
          printer.close()
        }
        val outputFiles = parseCompilerOutput(output, destinationDirectory)

        if (result.getCode() == 0) {
          storeTrackedOutputs(trackedOutputsFile, outputFiles)
        }

        result
      } else {
        compiler.exec(ctx.log.streams.err, allArgs*)
      }

    (exitCode.getCode(), exitCode.name())
  }

}
