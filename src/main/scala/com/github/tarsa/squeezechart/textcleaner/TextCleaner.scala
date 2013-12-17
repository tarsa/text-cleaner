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

    val marks = List("project gutenberg", "etext", "etexts", "ebook", "ebooks",
      "small print").toArray

    for ((name, path) <- nameToPath) {
      val content = Files.readAllLines(path, Charset.forName("ISO-8859-1"))
      val markedLines: ArrayBuffer[Int] = findMarkedLines(content, marks)
      val (firstContentLine, lastContentLine) =
        findMarkedBounds(content, markedLines)
      if (firstContentLine <= lastContentLine) {
        val subContent = content.subList(firstContentLine, lastContentLine + 1)
        writeContents(subContent, getPath(newRoot, nameToBook, name))
      }
    }
  }

  def getPath(root: Path, nameToBook: Map[String, Book], name: String) = {
    val bookOption = nameToBook.get(name)
    if (bookOption.isEmpty || bookOption.get.Language.isEmpty) {
      root.resolve("unknown").resolve(name)
    } else {
      root.resolve(bookOption.get.Language).resolve(name)
    }
  }

  def writeContents(contents: java.util.List[String], path: Path) {
    val sb = new StringBuilder()
    var lastSeparator = true
    for (line <- contents) {
      for (char <- line) {
        if (isNormalLetter(char)) {
          if (lastSeparator) {
            sb.append('`')
          }
          sb.append(char.toLower)
        }
        lastSeparator = !isNormalLetter(char)
      }
      lastSeparator = true
    }
    Files.createDirectories(path.getParent)
    Files.write(path, sb.toString().getBytes)
  }

  def findMarkedBounds(content: java.util.List[String],
                       markedLines: ArrayBuffer[Int]): (Int, Int) = {
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

  def findMarkedLines(content: java.util.List[String], marks: Array[String]):
  ArrayBuffer[Int] = {
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
    markedLines
  }

  def isNormalLetter(ch: Char) = {
    ch.isLower || ch.isUpper
  }

  def loadBooks(root: Path) = {
    val masterListFile = root.resolve("master_list.csv")
    val masterListLines = Files.readAllLines(masterListFile,
      Charset.defaultCharset())
    masterListLines.drop(5).map(Book.parse).toList
  }
}
