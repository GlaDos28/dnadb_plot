package ru.bmstu.bioinformatics.scoring

import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

import scala.collection.mutable

/** Represents symmetrical weight matrix, where keys are strings of a given size. */
object SubstringWeightMatrix {

  type SubstringWeightMatrix = Map[(String, String), Int]

  /** In order to satisfy symmetry property for [[SubstringWeightMatrix]], [[KeyMatrix]] is assumed to be symmetrical as well*/
  def apply(m: KeyMatrix, size: Int = 2): SubstringWeightMatrix = {
    ???
  }

  def allStrings(chars: Iterable[Char], rem: Int, acc: List[String]): List[String] = {
    if (rem > 0) {
      val newAcc = for {
        s <- acc
        c <- chars
      } yield s + c
      allStrings(chars, rem - 1, newAcc)
    } else {
      acc
    }
  }

  private def apply(keys: Iterable[Char], curr: String, rem: Int, acc: mutable.HashMap[(String, String), Int]): Unit = {
    if (rem > 0) {
      keys.foreach { c =>
        apply(keys, curr + c, rem - 1, acc)
      }
    }
  }
}
