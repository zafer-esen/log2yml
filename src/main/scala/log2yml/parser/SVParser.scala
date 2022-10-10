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

/**
 * Example input this parser expects:
 * ../sv-benchmarks/c/busybox-1.22.0/basename-1.yml; expected: false(valid-deref); status: ERROR (1); walltime: 0.4023271379992366s
 */
object SVParser {
  def apply(fileName: String,
            outputLines: Iterator[String]): (Summary, Seq[RunInfo]) = {
    var toolName: String = fileName
    var toolVersion: String = ""
    var toolOptions: String = ""
    var cpuTimeLimit: String = "900 s"
    var wallTimeLimit: String = "900 s"
    var os: String = ""
    var cpuModel: String = ""
    var cpuCount: String = "0"
    var architecture: String = ""
    var memTotal: String = "0"
    var startDate: String = "2022-01-01 00:00:00"
    var scriptDir: String = ""
    var notes: String = ""

    val runInfos: Seq[RunInfo] =
      (for ((line, lineNo) <- outputLines.zipWithIndex) yield {
        val values = line.split(';')
        if (values.length != 4)
          throw new Exception("Do not know how to parse the line " + line + " at " +
            toolName + ": " + lineNo)
        RunInfo(
          bmName = values(0),
          expected = values(1).drop(" expected: ".length),
          toolOutput = List(values(2).drop(" status: ".length)),
          duration = values(3).drop(" walltime: ".length)
        )
      }).toSeq

    (Summary(toolName, toolVersion, toolOptions, cpuTimeLimit, wallTimeLimit, os,
      cpuModel, cpuCount, architecture, memTotal, startDate, scriptDir, notes),
      runInfos)
  }
}