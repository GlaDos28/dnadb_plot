package ru.bmstu.bioinformatics.algo_legacy

import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

object SmithWatermanRaw {

  def sw(scoreTable: KeyMatrix, s1: String, s2: String): Int = {
    val gapPenalty = -1

    var res = 0

    val m = Array.fill(s1.length, s2.length)(0)

    for {
      i <- 0 until s1.length
      j <- 0 until s2.length
    } {
      val left    = if (j == 0) 0 else m(i)(j - 1)
      val top     = if (i == 0) 0 else m(i - 1)(j)
      val topLeft = if (i == 0 || j == 0) 0 else m(i - 1)(j - 1)
      val score = math.max(math.max(left + gapPenalty, top + gapPenalty), topLeft + scoreTable(s1(i))(s2(j)))

      m(i)(j) = score

      if (i == s1.length - 1 || j == s2.length - 1) {
        res = math.max(score, res)
      }
    }

    res
  }
}
