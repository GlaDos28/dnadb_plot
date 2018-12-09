package ru.bmstu.bioinformatics.scoring

import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

/** Represents match matrix, where keys are strings of a given size. */
object SubstringMatchMatrix {

  type SubstringMatchMatrix = Map[String, Int]

  def apply(m: KeyMatrix, size: Int = 2): SubstringMatchMatrix = {
    val chars = m.keys
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

  private def apply(m: KeyMatrix, keys: Iterable[String]): SubstringMatchMatrix = {
    val init = for {
      s1 <- keys
    } yield {
      s1 -> computeScore(m, s1, s1)
    }

    init.toMap
  }

  private def computeScore(m: KeyMatrix, s1: String, s2: String): Int = {
    s1.zip(s2).foldLeft(0) {
      case (acc, (c1, c2)) => acc + m(c1)(c2)
    }
  }
}
