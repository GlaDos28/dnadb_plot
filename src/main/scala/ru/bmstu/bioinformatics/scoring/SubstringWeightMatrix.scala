package ru.bmstu.bioinformatics.scoring

import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

/** Represents symmetrical weight matrix, where keys are strings of a given size. */
object SubstringWeightMatrix {

  type SubstringWeightMatrix = Map[(String, String), Int]

  /** In order to satisfy symmetry property for [[SubstringWeightMatrix]], [[KeyMatrix]] is assumed to be symmetrical as well*/
  def apply(m: KeyMatrix, size: Int = 2): SubstringWeightMatrix = {
    val chars = m.keys.map(_._1)
    apply(m, allStrings(chars, size - 1, chars.map(_.toString)))
  }

  def allStrings(chars: Iterable[Char], rem: Int, acc: Iterable[String]): Iterable[String] = {
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

  private def apply(m: KeyMatrix, keys: Iterable[String]): SubstringWeightMatrix = {
    println(keys.size)
    val init = for {
      s1 <- keys
      s2 <- keys if s2 >= s1
    } yield {
      (s1, s2) -> computeScore(m, s1, s2)
    }

    val map = init.toMap
    map.withDefault {
      case (s1, s2) if s2 < s1 => map(s2, s1)
    }
  }

  private def computeScore(m: KeyMatrix, s1: String, s2: String): Int = {
    s1.zip(s2).foldLeft(0) {
      case (acc, (c1, c2)) => acc + m((c1, c2))
    }
  }
}
