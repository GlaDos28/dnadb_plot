package ru.bmstu.bioinformatics.database.converted

import java.io.File
import java.nio.ByteBuffer

import boopickle.Default._
import ru.bmstu.bioinformatics.Utils

import scala.io.Source

object FileReader {

  lazy val defaultFile: File = Utils.resourceFile("converted.db")

  def default: Iterator[DbEntry] = apply(defaultFile)

  def apply(file: File): Iterator[DbEntry] = {
    Source.fromFile(file)
      .getLines()
      .map(s => Unpickle[DbEntry].fromBytes(ByteBuffer.wrap(s.getBytes)))
  }
}
