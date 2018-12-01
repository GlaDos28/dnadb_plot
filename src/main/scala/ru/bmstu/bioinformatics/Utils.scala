package ru.bmstu.bioinformatics

import java.io.File

object Utils {

  def resourceFile(fileName: String): File = {
    new File(getClass.getClassLoader.getResource(fileName).toURI)
  }
}
