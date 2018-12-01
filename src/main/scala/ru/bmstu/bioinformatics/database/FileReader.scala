package ru.bmstu.bioinformatics.database

import java.io.File

import scala.io.Source

/** File reader for protein database */
object FileReader {

  case class InvalidFileFormatException(file: File) extends Throwable {
    override def getMessage: String = s"Invalid sequence format in file [$file]"
  }

  def apply(file: File): Iterator[DbEntry] = {
      groupedIterator(file).map { case (name, content) => DbEntry((name, content)) }
  }

  private def groupedIterator(file: File): Iterator[(String, String)] = {
    val baseIterator = Source.fromFile(file).getLines()

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
}
