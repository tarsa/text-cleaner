package com.github.tarsa.squeezechart.textcleaner

import java.nio.file.{Paths, Path}
import scala.collection.mutable.ArrayBuffer

case class Book(Title: String, Subtitle: String, AuthorFN: String,
                AuthorLN: String, Text: String, Html: String, CatMonth: String,
                CatYear: Int, Language: String)

object Book {
  def parse(line: String) = {
    val raw = parseMasterListLineRaw(line)
    val unparsed: String = unparseRawStrings(raw)
    val reparsed: Array[String] = parseMasterListLineRaw(unparsed)
    if (raw.toSeq != reparsed.toSeq) {
      println("mismatch: ")
      println("  original: " + raw.deep.mkString(","))
      println("  unparsed: " + unparsed)
      println("  reparsed: " + reparsed.deep.mkString(","))
      throw new Exception()
    }
    mapRawStringsToBookDescription(raw)
  }

  private def parseMasterListLineRaw(line: String) = {
    val result = ArrayBuffer[String]()
    var i = 0
    while (i < line.length) {
      if (line(i) == '"') {
        var next = i
        do {
          next = line.indexOf('"', next + 1)
        } while ((next > 0 && line(next - 1) == '"' && next + 1 < line.length
          && line(next + 1) != ',') || (next + 1 < line.length
          && line(next + 1) == '"'))
        result += line.substring(i + 1, next)
        i = next + 1
      } else if (line(i).isDigit) {
        val nextCommaIndex = line.indexOf(',', i)
        result += line.substring(i, nextCommaIndex)
        i = nextCommaIndex
      } else if (line(i) == ',') {
        result += ""
      } else {
        throw new Exception()
      }
      if (i < line.length) {
        if (line(i) != ',') {
          throw new Exception()
        } else {
          i += 1
        }
      }
    }
    if (line(line.length - 1) == ',') {
      result += ""
    }
    result.toArray
  }

  private def unparseRawStrings(strings: Array[String]) = {
    val result = StringBuilder.newBuilder
    for (i <- 0 until strings.length) {
      if (i != 0) {
        result += ','
      }
      if (i == 7 || strings(i).isEmpty) {
        result ++= strings(i)
      } else {
        result ++= '"' + strings(i) + '"'
      }
    }
    result.toString()
  }

  private def mapRawStringsToBookDescription(strings: Array[String]) = {
    if (strings.length != 9) {
      throw new Exception()
    }
    if (strings(7).isEmpty) {
      strings(7) = "0"
    }
    Book(strings(0), strings(1), strings(2), strings(3), strings(4),strings(5),
      strings(6), Integer.valueOf(strings(7)), strings(8))
  }
}
