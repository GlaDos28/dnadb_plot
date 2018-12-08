package ru.bmstu.bioinformatics

import java.net.URL

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Utils {

  def resourceURL(fileName: String): URL = {
    getClass.getClassLoader.getResource(fileName)
  }

  implicit class AwaitSyntax[E](f: Future[E]) {
    def await(): E = {
      Await.result(f, Duration.Inf)
    }
  }

}
