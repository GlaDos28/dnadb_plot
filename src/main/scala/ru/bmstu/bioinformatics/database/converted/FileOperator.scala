package ru.bmstu.bioinformatics.database.converted

import java.io._
import java.nio.ByteBuffer

import ru.bmstu.bioinformatics.Utils
import boopickle.Default._

import scala.io.Source

object FileOperator {

  private val encoding = "UTF-8"

  lazy val defaultFile: File = Utils.resourceFile("converted.db")

  def readDefault: Iterator[DbEntry] = read(defaultFile)

  def read(file: File): Iterator[DbEntry] = {
    Source.fromFile(file)
      .getLines()
      .map(s => Unpickle[DbEntry].fromBytes(ByteBuffer.wrap(s.getBytes(encoding))))
  }

  def write(file: File)(db: Iterator[DbEntry]): Unit = {
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new BufferedWriter(new FileWriter(file))
    try {
      db.foreach { e =>
        writer.write(new String(Pickle.intoBytes(e).array(), encoding))
        writer.newLine()
        writer.flush()
      }
    } finally  {
      writer.close()
    }
  }
}
