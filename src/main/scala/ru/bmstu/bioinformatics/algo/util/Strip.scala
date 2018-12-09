package ru.bmstu.bioinformatics.algo.util

import ru.bmstu.bioinformatics.algo.input.SeqPair
import ru.bmstu.bioinformatics.algo.output.AlignResult
import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

import scala.collection.mutable.ListBuffer

class Strip(diags: IndexedSeq[Diagonal]) {
  def smithWatermanScore(gapPenalty: Int)
                        (seqPair: SeqPair, scoreTable: KeyMatrix): AlignResult = {
    val topBound    = rightOffset
    val bottomBound = leftOffset
    val maxWidth    = leftOffset - rightOffset

    var cnt = 0
    var i = math.max(topBound, 0)

    var curFirstColInd = math.max(-bottomBound, 0)
    var curLastColInd  = math.max(-topBound,    0)

    var maxScore = 0
    var maxScoreLen = 0

    /* First row elements */

    var rowScore    = Array.fill(maxWidth + 4)(0)
    val newRowScore = Array.fill(maxWidth + 4)(0)

    /* --- */

    while (i < seqPair.s1.length && curFirstColInd < seqPair.s2.length) {
      newRowScore(0) = -1000000 /* To prevent using non-strip elements */

      for (j <- curFirstColInd to curLastColInd) {
        val stScore      = scoreTable((seqPair.s1(i), seqPair.s2(j)))
        val leftScore    = newRowScore.last
        val topScore     = rowScore(j - curFirstColInd + 2)
        val topLeftScore = rowScore(j - curFirstColInd + 1)

        val score = math.max(math.max(
          leftScore    + gapPenalty,
          topScore     + gapPenalty),
          topLeftScore + stScore)

        if (i == seqPair.s1.length - 1 || j == seqPair.s2.length - 1) {
          maxScore = math.max(score, maxScore)
        }

        newRowScore(j - curFirstColInd + 1) = score
      }

      newRowScore(curLastColInd - curFirstColInd + 2) = -1000000 /* To prevent using non-strip elements */
      newRowScore(curLastColInd - curFirstColInd + 3) = -1000000
      rowScore = newRowScore

      cnt += 1
      i   += 1
      curFirstColInd = math.max(-bottomBound + cnt, 0)
      curLastColInd  = math.min(math.max(-topBound + cnt, 0), seqPair.s2.length - 1)
    }

    AlignResult(maxScore)
  }

  def leftOffset: Int = diags.head.offset

  def rightOffset: Int = diags.last.offset
}

object Strip {
  def scanlineDiagsFitStrip(w: Int)(diags: List[Diagonal]): List[Strip] = {
    val sortedDiags = diags.sortWith(_.offset < _.offset)
    val resBuffer = ListBuffer[Strip]()

    var strip = Vector[Diagonal](sortedDiags.head)
    var follow = 1

    /* Util functions */

    val fillUntilPossible = () => {
      while (follow < sortedDiags.size && sortedDiags(follow).offset - strip.head.offset + 1 <= w) {
        strip :+= sortedDiags(follow)
        follow += 1
      }
    }

    val removeUntilEnough = () => strip = strip.dropWhile(strip.last.offset - _.offset + 1 > w)

    /* --- */

    fillUntilPossible()
    resBuffer += new Strip(strip)

    while (follow < sortedDiags.size) {
      strip :+= sortedDiags(follow)
      follow += 1

      removeUntilEnough()
      fillUntilPossible()

      resBuffer += new Strip(strip)
    }

    resBuffer.toList
  }
}
