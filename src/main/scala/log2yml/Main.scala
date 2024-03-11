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
        |""".stripMargin

    if (args.isEmpty) {
      println(usage); return
    }

    val arglist = args.toList

    // default args
    var inPath = ""
    var outFileName = ""
    var svInput = false
    var isDirectory = false

    def parseOptions(list: List[String]): Unit = {
      list match {
        case Nil => // nothing
        case "-out" :: str :: tail =>
          outFileName = str
          parseOptions(tail)
        case "-sv" :: tail =>
          svInput = true
          parseOptions(tail)
        case "-dir" :: tail =>
          isDirectory = true
          parseOptions(tail)
        case string :: Nil =>
          inPath = string
        case option :: _ =>
          println("Unknown option: " + option + "\n")
      }
    }

    parseOptions(arglist)
    if (outFileName.isEmpty) {
      outFileName = if (isDirectory) "" else inPath + ".yml"
    }

    if (inPath.isEmpty) {
      println("An input path must be provided.\n")
      println(usage)
      return
    }

    // Set output filename based on directory name or input filename, if not explicitly specified
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
          if (svInput)
            parser.SVParser(file.getName, lines)
          else
            parser.LogParser(lines.mkString("\n"))
        lastSummary = summary
        // todo: do some checks here regarding summary
        bmRuns
      }
      println(aggregatedResults.length + " benchmark runs found for the tool " +
              lastSummary.toolName + ".")
      // Process and output aggregated results for all files
      outputYaml(aggregatedResults, lastSummary, outFileName)
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
      outputYaml(bmRuns, summary, outFileName) // You will implement this function
    }

    println("Done!")
  }
}