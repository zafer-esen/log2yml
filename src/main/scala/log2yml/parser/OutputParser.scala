package log2yml.parser

import log2yml.Benchmarks._
import scala.collection.mutable.{HashSet => MHashSet}

// todo: these parsers need to recognize much more output!

trait OutputParser {
  def apply(outputLines : Seq[String]) : Result
}
class EldaricaOutputParser extends OutputParser {

  def apply(outputLines : Seq[String]) : Result = {
    var _isSat = false
    var _isUnsat = false
    var _isUnknown = false
    var _isError = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "error" =>
          _isError = true;
        case _ if line contains "unsat" =>
          if (_isSat || _isUnknown) throw new Exception(
            "e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if line contains "sat" =>
          if (_isUnsat || _isUnknown) throw new Exception(
            "e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _ if line contains "unknown" =>
          if (_isSat || _isUnsat) throw new Exception(
            "e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ => // nothing
      }
    }

    val result: Result = if (_isError) {
      val errorMsg = outputLines.mkString("\n")
      val errorTypes = new MHashSet[ErrorType.Value]
      for(line <- outputLines) {
        line match {
          case _ if line contains "Predicate generation failed" =>
            errorTypes += ErrorType.Solve
          case _ =>
            errorTypes += ErrorType.Other // todo: detect other errors
          case _ if line contains "error" =>
            errorTypes += ErrorType.Other // todo: detect other errors
          case _ => // nothing
        }
      }
      Error(errorTypes.toSet, errorMsg)
    } else if (_isSat) {
      True
    } else if (_isUnsat) {
      False
    } else if (_isUnknown) {
      Unknown
    } else throw new Exception(
      "e4: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}

class Z3OutputParser extends OutputParser {

  def apply(outputLines : Seq[String]) : Result = {
    var _isSat = false
    var _isUnsat = false
    var _isUnknown = false
    var _isError = false

    for (line <- outputLines) {
      line match {
        case _ if line contains "error" =>
          _isError = true;
        case _ if line contains "unsat" =>
          if (_isSat || _isUnknown) throw new Exception(
            "e1: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnsat = true
        case _ if line contains "sat" =>
          if (_isUnsat || _isUnknown) throw new Exception(
            "e2: cannot determine benchmark output from lines: " + outputLines)
          else
            _isSat = true
        case _ if (line contains "unknown")=>
          if (_isSat || _isUnsat) throw new Exception(
            "e3: cannot determine benchmark output from lines: " + outputLines)
          else
            _isUnknown = true
        case _ => // nothing
      }
    }

    val result: Result = if (_isError) {
      val errorMsg = outputLines.mkString("\n")
      val errorTypes = new MHashSet[ErrorType.Value]
      for(line <- outputLines) {
        line match {
          case _ if line contains "unknown sort" =>
            errorTypes += ErrorType.Encode
          case _ if line contains "unsupported" =>
            errorTypes += ErrorType.Parse
          case _ if line contains "error" =>
            errorTypes += ErrorType.Other // todo: detect other errors
          case _ => // nothing
        }
      }
      Error(errorTypes.toSet, errorMsg)
    } else if (_isSat) {
      True
    } else if (_isUnsat) {
      False
    } else if (_isUnknown) {
      Unknown
    } else throw new Exception(
      "e4: cannot determine benchmark output from lines: " + outputLines)

    result

  }

}