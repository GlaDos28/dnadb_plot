package ru.bmstu.bioinformatics.database.initial

import java.io.File
import java.net.URL

import scala.io.Source

/** File reader for protein database */
object FileReader {

  def read(url: URL): Iterator[DbEntry] = {
    groupedIterator(url).map { case (name, content) => DbEntry((name, content)) }
  }

  private def groupedIterator(url: URL): Iterator[(String, String)] = {
    val baseIterator = Source.fromURL(url).getLines()

    new Iterator[(String, String)] {
      private var previousCaption: String = _

      override def hasNext: Boolean = baseIterator.hasNext

      override def next(): (String, String) = {
        val caption = if (previousCaption != null) previousCaption else baseIterator.next()
        val builder = new StringBuilder()
        var currLine = ""
        while (baseIterator.hasNext && !isCaptionLine(currLine)) {
          currLine = baseIterator.next()
          if (!isCaptionLine(currLine)) {
            builder.append(currLine.toUpperCase())
          }
        }
        previousCaption = currLine
        (caption, builder.toString())
      }
    }
  }

  private def isCaptionLine(str: String): Boolean = str.startsWith(">")

  case class InvalidFileFormatException(file: File) extends Throwable {
    override def getMessage: String = s"Invalid sequence format in file [$file]"
  }
}
