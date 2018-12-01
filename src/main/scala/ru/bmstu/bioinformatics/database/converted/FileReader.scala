package ru.bmstu.bioinformatics.database.converted

import java.io.File
import java.nio.ByteBuffer

import scala.io.Source
import boopickle.Default._
import ru.bmstu.bioinformatics.Utils

object FileReader {

  def default: Iterator[DbEntry] = apply(Utils.resourceFile("converted.db"))

  def apply(file: File): Iterator[DbEntry] = {
    Source.fromFile(file)
      .getLines()
      .map(s => Unpickle[DbEntry].fromBytes(ByteBuffer.wrap(s.getBytes)))
  }
}
