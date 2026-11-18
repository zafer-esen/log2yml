/**
 * Copyright (c) 2022 Zafer Esen. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * * Neither the name of the authors nor the names of their
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package log2yml.parser

import log2yml.Benchmarks._

import scala.util.matching.Regex
import scala.util.parsing.combinator._


object LogParser extends RegexParsers {
  // holds summary data located at the top of the input file
  // the log file contains a sequence of benchmark runs after the summary
  // output might contain many lines

  override val whiteSpace: Regex = """[ \t]+""".r

  private def parseLogFile: Parser[(Summary, Seq[RunInfo])] = {
    overviewFields ~ benchmarkRuns
  } ^^ {s => (s._1, s._2)}

////////////////////////////////////////////////////////////////////////////////
// Benchmark run information

  // cpu usage / mem usage / num processes left behind
  // and the number of processes left behind by the run and killed
  def measurementLabel: Parser[String] = "cpu-time" | "max-rss" | "leftovers"

  // benchmark run information can contain one of these labels (excluding output)
  def runFieldIdentifier: Parser[String] =
    "filename" | "expected" | "duration" | measurementLabel

  // each line can be runFieldIdentifier : ...
  def runFieldLine: Parser[(String, Option[String])] =
    fieldLine(runFieldIdentifier)

  def measurementLine: Parser[(String, Option[String])] =
    fieldLine(measurementLabel)

  private def fieldLine(label : Parser[String]) : Parser[(String, Option[String])] =
    label ~ ":" ~
       opt(not((runFieldIdentifier | "output") ~ ":") ~> (literalLine | literalSingleLine))^^
     {s => (s._1._1, s._2)}

  // we also have the tool output, starting with "output", and can have many lines

  def runOutputLine: Parser[String] = {
    not(runFieldLine) ~> literalLine ^^ { s => s }
  }

  //todo: parse output lines
  val runOutputField : Parser[(String, Seq[String])] =
    "output" ~ ":" ~ runOutputLine.+ ^^ { s => (s._1._1, s._2)}

  // each benchmark run currently has 4 lines (messy, clean up)
  val benchmarkRun: Parser[RunInfo] =
    runFieldLine ~ opt(runFieldLine) ~ runOutputField ~ rep(measurementLine) ~
    runFieldLine  ^^ {
      case name ~ maybeExpected ~ output ~ measurements ~ duration =>
        val measured = measurements.toMap
        RunInfo(name._2.getOrElse(""),
                maybeExpected.getOrElse(("",Some("")))._2.getOrElse(""),
                output._2,
                duration._2.getOrElse(""),
                measured.get("cpu-time").flatten,
                measured.get("max-rss").flatten)
    }

  val benchmarkRuns : Parser[Seq[RunInfo]] = benchmarkRun.+

////////////////////////////////////////////////////////////////////////////////
// Overview / summary fields

  val overviewFieldLabel: Parser[String] =
    "tool-name" | "tool-version" | "tool-options" | "cpu-time-limit" |
      "wall-time-limit"  | "os" | "cpu-model" | "cpu-count" | "architecture" |
    "mem-total" | "start-date" | "script-dir" | "notes" | "hostname" |
    "slurm-job-id" | "slurm-array-task-id" | "mem-limit"

  def overviewField : Parser[(String, String)] =
    overviewFieldLabel ~ ":" ~ overviewFieldValue ^^ {
      case s => (s._1._1, s._2)
    }
  def overviewFieldValue : Parser[String] = literalLine

  val overviewFields : Parser[Summary] = overviewField.+ ^^ {
    case fields =>
      var toolName      : String = ""
      var toolVersion   : String = ""
      var toolOptions   : String = ""
      var cpuTimeLimit  : String = ""
      var wallTimeLimit : String = ""
      var os            : String = ""
      var cpuModel      : String = ""
      var cpuCount      : String = ""
      var architecture  : String = ""
      var memTotal      : String = ""
      var startDate     : String = ""
      var scriptDir     : String = ""
      var notes         : String = ""
      var hostname         : Option[String] = None
      var slurmJobId       : Option[String] = None
      var slurmArrayTaskId : Option[String] = None
      var memLimit         : Option[String] = None

      for ((label, value) <- fields) {
        label match {
          case "tool-name"        => toolName = value
          case "tool-version"     => toolVersion = value
          case "tool-options"     => toolOptions = value
          case "cpu-time-limit"   => cpuTimeLimit = value
          case "wall-time-limit"  => wallTimeLimit = value
          case "os"               => os = value
          case "cpu-model"        => cpuModel = value
          case "cpu-count"        => cpuCount = value
          case "architecture"     => architecture = value
          case "mem-total"        => memTotal = value
          case "start-date"       => startDate = value
          case "script-dir"       => scriptDir = value
          case "notes"            => notes = value
          case "hostname"            => hostname = Some(value)
          case "slurm-job-id"        => slurmJobId = Some(value)
          case "slurm-array-task-id" => slurmArrayTaskId = Some(value)
          case "mem-limit"           => memLimit = Some(value)
          case _ => throw new Exception("Unknown field: " + label)
        }
      }
      Summary(toolName,toolVersion,toolOptions,cpuTimeLimit,wallTimeLimit,os,
        cpuModel,cpuCount,architecture,memTotal,startDate,scriptDir,notes,
        hostname,slurmJobId,slurmArrayTaskId,memLimit)
  }

////////////////////////////////////////////////////////////////////////////////
// Other tokens

  def unboundedInteger: Parser[String] = {
    "[1-9][0-9]*".r ^^ { str => str }
  }

// string literal, any sequence of characters except new line, then one or more new lines at the end
  def literalLine: Parser[String] = """[^\n]*(\r?\n)+""".r ^^ {
    case s => s.trim
  }

  // string literal, any sequence of characters except new line, then one or more new lines at the end
  def literalSingleLine: Parser[String] = """[^\n]*(\r?\n)?""".r ^^ {
    case s => s.trim
  }

  // the log up to the last complete run, None if no run is complete or
  // nothing follows the last one
  private def upToLastCompleteRun(annotation : String) : Option[String] = {
    val lines = annotation.split("\n", -1)
    val last  = lines.lastIndexWhere(l => l.trim.startsWith("duration") &&
                                          l.contains(":"))
    if (last < 0 || last == lines.length - 1) None
    else Some(lines.take(last + 1).mkString("\n"))
  }

  def apply (annotation : String) : (Summary, Seq[RunInfo]) = {
    parse(phrase(rep1(parseLogFile)), annotation) match {
      case Success(result, _) => result.head
      case NoSuccess(msg, input) =>
        // the log of a cancelled job ends with a cut-off run
        upToLastCompleteRun(annotation) match {
          case Some(prefix) =>
            parse(phrase(rep1(parseLogFile)), prefix) match {
              case Success(result, _) => result.head
              case NoSuccess(msg2, input2) => throw new Exception(
                msg2 + " at line " + input2.pos.line + " column " + input2.pos.column)
            }
          case None => throw new Exception(
            msg + " at line " + input.pos.line + " column " + input.pos.column)
        }
    }
  }
}
