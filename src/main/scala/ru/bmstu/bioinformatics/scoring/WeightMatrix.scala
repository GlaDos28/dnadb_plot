package ru.bmstu.bioinformatics.scoring

import java.net.URL

import ru.bmstu.bioinformatics.Utils

import scala.collection.mutable
import scala.io.Source

/** Represents symmetrical weight matrix */
object WeightMatrix {

  type KeyMatrix = Map[Char, Map[Char, Int]]

  def readDefault: KeyMatrix = fromResource("protein.mtx")

  def fromResource(name: String): KeyMatrix = {
    fromURL(Utils.resourceURL(name))
  }

  /** Any lines starting with # are discarded
    * The first line (excluding comments) should contain only space separated latin characters or
    * an asterisk (any not-stated latin character)
    * The body of the (square) matrix should consist of lines starting with a latin character on
    * an asterisk which is followed by a series of numbers corresponding to the size of the matrix.
    *
    * @return symmetrical weight matrix of keys
    */
  def fromURL(fileInput: URL): KeyMatrix = {
    val builder = mutable.AnyRefMap[(Char, Char), Int]()
    val names :: body = Source.fromURL(fileInput)
      .getLines()
      .map(_.trim)
      .filter(!_.startsWith("#"))
      .toList

    val nameArray = splitBySpaces(names).map { n =>
      assert(n.length == 1)
      n.head
    }

    assert(nameArray.length == body.length)
    val excludedSet = ('A' to 'Z').toSet.diff(nameArray.toSet)

    body.foreach { line =>
      //First element of row is a letter
      val lineArray = splitBySpaces(line)
      assert(lineArray(0).length == 1)
      val rowName = lineArray(0).head

      lineArray
        .drop(1)
        .map(_.toInt)
        .zipWithIndex
        .foreach { case (weight, i) =>
          (rowName, nameArray(i)) match {
            case ('*', '*') =>
              for {
                n1 <- excludedSet
                n2 <- excludedSet
              } {
                builder.update((n1, n2), weight)
              }

            case ('*', colName) =>
              excludedSet.foreach(n => builder.update((n, colName), weight))

            case (_, '*') =>
              excludedSet.foreach(n => builder.update((rowName, n), weight))

            case (_, colName) =>
              builder.update((rowName, colName), weight)
          }
        }
    }

    builder
      .toMap
      .groupBy { case ((c1, _), _) => c1 }
      .map { case (c1, seq) =>
        c1 -> seq.map { case ((_, c2), s) => c2 -> s }
      }
  }

  private def splitBySpaces(str: String) = str.split("\\s+")
}
