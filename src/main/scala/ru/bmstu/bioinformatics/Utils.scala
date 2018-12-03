package ru.bmstu.bioinformatics

import java.io.File

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Utils {

  def resourceFile(fileName: String): File = {
    new File(getClass.getClassLoader.getResource("").getPath, fileName)
  }

  implicit class AwaitSyntax[E](f: Future[E]) {
    def await(): E = {
      Await.result(f, Duration.Inf)
    }
  }

}
