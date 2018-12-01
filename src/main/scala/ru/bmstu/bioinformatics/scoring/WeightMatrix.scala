package ru.bmstu.bioinformatics.scoring

import java.io.File

import scala.collection.mutable
import scala.io.Source

/** Represents symmetrical weight matrix */
object WeightMatrix {

  type KeyMatrix = Map[(Char, Char), Int]

  def readDefault: KeyMatrix = fromResource("protein.mtx")

  def fromResource(name: String): KeyMatrix = {
    fromFile(getResourceFile(name))
  }

  /** Any lines starting with # are discarded
    * The first line (excluding comments) should contain only space separated latin characters or
    * an asterisk (any not-stated latin character)
    * The body of the (square) matrix should consist of lines starting with a latin character on
    * an asterisk which is followed by a series of numbers corresponding to the size of the matrix.
    * @return symmetrical weight matrix of keys
   */
  def fromFile(fileInput: File): KeyMatrix = {
    val builder = mutable.HashMap[(Char, Char), Int]()
    val names :: body = Source.fromFile(fileInput)
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
              } { builder.update((n1, n2), weight) }

            case ('*', colName) =>
              excludedSet.foreach(n => builder.update((n, colName), weight))

            case (_, '*') =>
              excludedSet.foreach(n => builder.update((rowName, n), weight))

            case (_, colName) =>
              builder.update((rowName, colName) , weight)
          }
        }
    }

    val init = builder.filterKeys(t => t._2 >= t._1).toMap

    init.withDefault {
      case (k1, k2) if k2 < k1 => init(k2, k1)
    }
  }

  private def splitBySpaces(str: String) = str.split("\\s+")

  private def getResourceFile(fileName: String): File = {
    new File(getClass.getClassLoader.getResource(fileName).toURI)
  }
}
