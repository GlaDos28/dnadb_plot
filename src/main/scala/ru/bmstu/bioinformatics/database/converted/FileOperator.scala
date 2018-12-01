package ru.bmstu.bioinformatics.database.converted

import java.io._
import java.nio.ByteBuffer

import boopickle.Default._
import ru.bmstu.bioinformatics.Utils

import scala.collection.mutable.ArrayBuffer

object FileOperator {

  private val chunkSize = 256

  lazy val defaultFile: File = Utils.resourceFile("converted.db")

  def readDefault: Iterator[DbEntry] = read(defaultFile)

  def read(file: File): Iterator[DbEntry] = {
    val reader = new BufferedInputStream(new FileInputStream(file))
    val bin = ArrayBuffer.empty[Byte]
    try {
      val buf = new Array[Byte](chunkSize)
      while (reader.available() > 0) {
        val readCount = reader.read(buf)
        bin ++= buf.take(readCount)
      }
    } finally {
      reader.close()
    }
    Unpickle.apply[List[DbEntry]].fromBytes(ByteBuffer.wrap(bin.toArray)).iterator
  }

  def write(file: File)(db: Iterator[DbEntry]): Unit = {
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new BufferedOutputStream(new FileOutputStream(file))
    try {
      val bin = Pickle.intoBytes(db.toList).array()
      bin.grouped(chunkSize).foreach { chunk =>
        writer.write(chunk)
        writer.flush()
      }
    } finally  {
      writer.close()
    }
  }
}
