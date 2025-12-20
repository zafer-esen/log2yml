package log2yml

import java.io.{BufferedWriter, FileWriter, File}
import Benchmarks.MyYamlProtocol._
import log2yml.Benchmarks.Result

import scala.io.Source
import net.jcazevedo.moultingyaml._
import log2yml.parser._

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val usage =
      """Usage: log2yml [-out filename] [-sv] [-dir] inPath
        |  inPath          : input file or directory to process
        |  [-out filename] : output filename (default: based on inPath or directory name)
        |  [-sv]           : parse output gathered from SV-COMP published results (default: false)
        |  [-dir]          : indicates that inPath is a directory containing multiple .log1 files
        |  [-notes str]    : overwrite notes field in the generated yml file using str
        |""".stripMargin

    if (args.isEmpty) {
      println(usage); return
    }

    val arglist = args.toList

    // default args
    var inPath = ""
    var outFileName = ""
    var overwriteNotes = false
    var overwriteNoteString = ""
    var svInput = false
    var isDirectory = false

    def parseOptions(list: List[String]): List[String] = list match {
      case Nil => Nil
      case "-out" :: str :: tail =>
        outFileName = str
        parseOptions(tail)
      case "-notes" :: str :: tail =>
        overwriteNoteString = str
        overwriteNotes = true
        parseOptions(tail)
      case "-sv" :: tail =>
        svInput = true
        parseOptions(tail)
      case "-dir" :: tail =>
        isDirectory = true
        parseOptions(tail)
      case option :: tail if option.startsWith("-") =>
        println(s"Unknown option: $option")
        parseOptions(tail)
      case nonOption :: tail =>
        nonOption :: tail
    }

    val remainingArgs = parseOptions(arglist)
    remainingArgs match {
      case List(inPathArg) =>
        inPath = inPathArg
      case _ =>
        if (remainingArgs.isEmpty) {
          println("Error: Missing input path.")
        } else {
          println(s"Error: Expected exactly one input path, but got ${remainingArgs.size} arguments: ${remainingArgs.mkString(", ")}")
        }
        println(usage)
        return
    }

    if (outFileName.isEmpty) {
      outFileName = if (isDirectory) "" else inPath + ".yml"
    }

    // Set output filename based on directory name or input filename if not specified
    if (outFileName.isEmpty) {
      val file = new File(inPath)
      if (isDirectory) {
        outFileName = s"${file.getAbsolutePath}/${file.getName}.yml"
      } else {
        outFileName = s"${file.getAbsolutePath}.yml"
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Processing

    def outputYaml(infoes: Seq[Benchmarks.RunInfo],
                   summary: Benchmarks.Summary,
                   str: String) = {
      //    val outputParser : OutputParser =
      //    summary.toolName match {
      //      case s if s.toLowerCase contains "eld" => new EldaricaOutputParser
      //      case s if s.toLowerCase contains "z3"  => new Z3OutputParser
      //    }
      //
      //    import Benchmarks.MyYamlProtocol._
      //
      //    val runResults  = bmRuns.map(bmRun => outputParser(bmRun
      //    .toolOutput))
      //    val runsYaml = runResults.map(res => res.toYaml)

      //    inFile.close

      implicit val yamlPrinter : SnakeYamlPrinter =
        new SnakeYamlPrinter(flowStyle = Block) // Block: indentation,
      // Flow: explicit scopes

      val outFile = new File(outFileName)
      val bw      = new BufferedWriter(new FileWriter(outFile))

      println("Outputting YAML to " + outFileName + "...")

      val runsYaml     = infoes.map(run => run.toYaml).toYaml
      val documentYaml = YamlArray(summary.toYaml, runsYaml)

      bw.write(documentYaml)

      //    runsYaml.foreach(r => bw.write(r.print))

      bw.close()
    }

    if (isDirectory) {
      // Check if a custom output filename was provided; if not, set it based
      // on the directory name
      val dir = new File(inPath)
      if (!dir.exists || !dir.isDirectory) {
        println(s"The provided path $inPath is not a valid directory.")
        return
      }
      val logFiles = dir.listFiles().filter(_.getName.endsWith(".log1"))
      var lastSummary : Benchmarks.Summary = null
      val aggregatedResults = logFiles.flatMap{file =>
        val lines = Source.fromFile(file).getLines()
        val (summary, bmRuns) =
          try {
            if (svInput)
              parser.SVParser(file.getName, lines)
            else
              parser.LogParser(lines.mkString("\n"))
          } catch {
            case e : Throwable =>
              println("Error parsing file: " + file.getAbsolutePath)
              throw e
          }
        lastSummary = summary
        // todo: do some checks here regarding summary
        bmRuns
      }
      println(aggregatedResults.length + " benchmark runs found for the tool " +
              lastSummary.toolName + ".")
      // Process and output aggregated results for all files
      outputYaml(aggregatedResults,
                 lastSummary.withNotesIf(overwriteNoteString, overwriteNotes),
                 outFileName)
    } else {
      val lines = Source.fromFile(inPath).getLines()
      val (summary, bmRuns) =
        if (svInput)
          parser.SVParser(inPath, lines)
        else
          parser.LogParser(lines.mkString("\n"))
      // Process and output results for the single file
      println(bmRuns.length + " benchmark runs found for the tool " +
              summary.toolName + ".")
      outputYaml(bmRuns,
                 summary.withNotesIf(overwriteNoteString, overwriteNotes),
                 outFileName) // You will implement this function
    }

    println("Done!")
  }
}