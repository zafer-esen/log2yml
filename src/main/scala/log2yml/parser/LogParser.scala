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

  // benchmark run information can contain one of these labels (excluding output)
  def runFieldIdentifier: Parser[String] =
    "filename" | "expected" | "duration"

  // each line can be runFieldIdentifier : ...
  def runFieldLine: Parser[(String, String)] =
    runFieldIdentifier ~ ":" ~ (literalLine | literalSingleLine)^^ {s => (s._1._1, s._2)}

  // we also have the tool output, starting with "output", and can have many lines

  def runOutputLine: Parser[String] = {
    not(runFieldLine) ~> literalLine ^^ { s => s }
  }

  //todo: parse output lines
  val runOutputField : Parser[(String, Seq[String])] =
    "output" ~ ":" ~ runOutputLine.+ ^^ { s => (s._1._1, s._2)}

  // each benchmark run currently has 4 lines (messy, clean up)
  val benchmarkRun: Parser[RunInfo] =
    runFieldLine ~ runFieldLine ~ runOutputField ~ runFieldLine  ^^ {
      case name ~ expected ~ output ~ duration =>
        RunInfo(name._2, expected._2, output._2, duration._2)
    }

  val benchmarkRuns : Parser[Seq[RunInfo]] = benchmarkRun.+

////////////////////////////////////////////////////////////////////////////////
// Overview / summary fields

  val overviewFieldLabel: Parser[String] =
    "tool-name" | "tool-version" | "tool-options" | "cpu-time-limit" |
      "wall-time-limit"  | "os" | "cpu-model" | "cpu-count" | "architecture" |
    "mem-total" | "start-date" | "script-dir" | "notes"

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
          case _ => throw new Exception("Unknown field: " + label)
        }
      }
      Summary(toolName,toolVersion,toolOptions,cpuTimeLimit,wallTimeLimit,os,
        cpuModel,cpuCount,architecture,memTotal,startDate,scriptDir,notes)
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

  def apply (annotation : String) : (Summary, Seq[RunInfo]) = {
    parse(phrase(rep1(parseLogFile)), annotation) match {
      case NoSuccess(msg, input)  => throw new Exception(
        msg + " at line " + input.pos.line + " column " + input.pos.column)
      case Success(result, _) => result.head
    }
  }
}
