package ru.bmstu.bioinformatics.database.converted

import java.io.{BufferedWriter, File}

import boopickle.Default._

object FileWriter {

  def write(file: File)(db: Iterator[DbEntry]): Unit = {
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new BufferedWriter(new java.io.FileWriter(file))
    try {
      db.foreach { e =>
        writer.write(Pickle.intoBytes(e).asCharBuffer().array())
        writer.newLine()
      }
    } finally  {
      writer.close()
    }
  }
}
