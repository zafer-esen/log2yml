package log2yml

import java.io.{BufferedWriter, FileWriter}
import Benchmarks.MyYamlProtocol._
import log2yml.Benchmarks.Result

import scala.io.Source
import net.jcazevedo.moultingyaml._
import log2yml.parser._

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val usage =
"""Usage: log2yml inFileName [-out filename]
  inFileName      : input file to process
  [-out filename] : output filename (default: (inFileName).yml)
"""

    import java.io.File

    if (args.length == 0) {
      println(usage); return
    }
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    // default args
    var inFileName = ""
    var outFileName = ""

    def parseOptions(list: List[String]): Unit = {
      list match {
        case Nil => // nothing
        case "-out" :: str :: tail =>
          outFileName = str
          parseOptions(tail)
        case string :: Nil =>
          inFileName = string
          parseOptions(list.tail)
        case option :: _ =>
          println("Unknown option: " + option + "\n")
      }
    }

    parseOptions(arglist)
    if (outFileName.isEmpty) {
      outFileName = inFileName + ".yml"
    }

    if (inFileName.isEmpty) {
      println("An input filename must be provided.\n")
      println(usage)
      return
    }

    val inFile = Source.fromFile(inFileName)

    val lines = inFile.getLines()

    val (summary, bmRuns) = parser.LogParser(lines.mkString("\n"))

    println(bmRuns.length + " benchmark runs found for the tool " +
      summary.toolName + ".")

//    val outputParser : OutputParser =
//    summary.toolName match {
//      case s if s.toLowerCase contains "eld" => new EldaricaOutputParser
//      case s if s.toLowerCase contains "z3"  => new Z3OutputParser
//    }
//
//    import Benchmarks.MyYamlProtocol._
//
//    val runResults  = bmRuns.map(bmRun => outputParser(bmRun.toolOutput))
//    val runsYaml = runResults.map(res => res.toYaml)

//    inFile.close

    implicit val yamlPrinter: SnakeYamlPrinter =
      new SnakeYamlPrinter(flowStyle = Block) // Block: indentation, Flow: explicit scopes

    val outFile  = new File(outFileName)
    val bw = new BufferedWriter(new FileWriter(outFile))

    println("Outputting YAML to " + outFileName + "...")

    val runsYaml = bmRuns.map(run => run.toYaml).toYaml
    val documentYaml = YamlArray(summary.toYaml,runsYaml)

    bw.write(documentYaml)

//    runsYaml.foreach(r => bw.write(r.print))

    bw.close()

    println("Done!")

  }
}