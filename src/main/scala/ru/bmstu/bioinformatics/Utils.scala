package ru.bmstu.bioinformatics

import java.io.File

object Utils {

  def resourceFile(fileName: String): File = {
    println(fileName)
    println(getClass.getClassLoader.getResource(""))
    new File(getClass.getClassLoader.getResource("").getPath + fileName)
  }
}
