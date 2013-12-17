package com.github.tarsa.squeezechart.textcleaner

import java.nio.file.{Path, Files, Paths}
import scala.collection.convert.WrapAsScala._
import java.nio.charset.Charset
import org.apache.commons.lang3.StringUtils
import scala.collection.mutable.ArrayBuffer

object TextCleaner {
  def main(args: Array[String]) {
    val root = Paths.get(args(0))
    val newRoot = Paths.get(args(1))

    val nameToBook = loadBooks(root).filter(_.Text.endsWith(".txt")).map {
      book: Book => (book.Text, book)
    }.toMap

    val nameToPath = Files.newDirectoryStream(root, "etext??")
      .filter(Files.isDirectory(_)).flatMap {
      dir: Path =>
        Files.newDirectoryStream(dir, "*.txt").toList
    }.map(path => (path.getFileName.toString, path)).toMap

    val nameToContents = nameToPath.map {
      case (name: String, path: Path) =>
        (name, Files.readAllLines(path, Charset.forName("ISO-8859-1")))
    }.asInstanceOf[Map[String, java.util.List[String]]]

    val marks = List("project gutenberg", "etext", "etexts", "ebook", "ebooks",
      "small print").toArray

    for ((name, content) <- nameToContents) {
      val markedLines = ArrayBuffer[Int]()
      for ((line, number) <- content.zipWithIndex) {
        val lineLow = line.toLowerCase
        var i = 0
        while (i < lineLow.length) {
          val pos = StringUtils.indexOfAny(lineLow.substring(i), marks: _*)
          if (pos == -1) {
            i = lineLow.length
          } else {
            val mark = marks.
              filter(m => lineLow.substring(i).indexOf(m) == pos).
              maxBy(_.length)
            val prevGood = i + pos == 0 ||
              !isNormalLetter(line.charAt(i + pos - 1))
            val nextGood = i + pos + mark.length == line.length ||
              !isNormalLetter(line.charAt(i + pos + mark.length))
            val separated = prevGood && nextGood
            if (separated) {
              markedLines += number
              i = lineLow.length
            } else {
              i += pos + mark.length
            }
          }
        }
      }
      val (firstContentLine, lastContentLine) = {
        var lastLimit = content.length
        var lastTooFarLineIndex = markedLines.length - 1
        while (lastTooFarLineIndex >= 0 &&
          markedLines(lastTooFarLineIndex) + 50 > lastLimit) {
          lastLimit = markedLines(lastTooFarLineIndex)
          lastTooFarLineIndex -= 1
        }

        var firstLimit = -1
        var firstTooFarLineIndex = 0
        while (firstTooFarLineIndex < markedLines.length &&
          markedLines(firstTooFarLineIndex) - 50 < firstLimit) {
          firstLimit = markedLines(firstTooFarLineIndex)
          firstTooFarLineIndex += 1
        }

        var firstContentLine = firstLimit
        while (firstContentLine != -1 && firstContentLine < content.length
          && !content.get(firstContentLine).trim().isEmpty) {
          firstContentLine += 1
        }
        while (firstContentLine != -1 && firstContentLine < content.length
          && content.get(firstContentLine).trim().isEmpty) {
          firstContentLine += 1
        }
        if (firstContentLine == -1) {
          firstContentLine = 0
        }

        var lastContentLine = lastLimit
        while (lastContentLine != content.length && lastContentLine > 0
          && !content.get(lastContentLine).trim().isEmpty) {
          lastContentLine -= 1
        }
        while (lastContentLine != content.length && lastContentLine > 0
          && content.get(lastContentLine).trim().isEmpty) {
          lastContentLine -= 1
        }
        if (lastContentLine == content.length) {
          lastContentLine -= 1
        }

        (firstContentLine, lastContentLine)
      }
      if (firstContentLine <= lastContentLine) {
        val subContent = content.subList(firstContentLine, lastContentLine + 1)

        val path = nameToPath(name)
        val relative = root.relativize(path)
        val newPath = newRoot.resolve(relative)
        Files.createDirectories(newPath.getParent)
        Files.write(newPath, subContent, Charset.forName("ISO-8859-1"))
      } else {
        println(name)
      }
    }
  }

  def isNormalLetter(ch: Char) = {
    ch.isLower || ch.isUpper || ch.isTitleCase
  }

  def moreSubstring(s: String, start: Int, length: Int) = {
    val newStart = Math.max(0, start - 5)
    val newEnd = Math.min(s.length, start + length + 5)
    s.substring(newStart, newEnd)
  }

  def loadBooks(root: Path) = {
    val masterListFile = root.resolve("master_list.csv")
    val masterListLines = Files.readAllLines(masterListFile,
      Charset.defaultCharset())
    masterListLines.drop(5).map(Book.parse).toList
  }
}
