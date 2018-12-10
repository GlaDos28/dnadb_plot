package ru.bmstu.bioinformatics.algo.util

import ru.bmstu.bioinformatics.algo.input.SeqPair
import ru.bmstu.bioinformatics.algo.output.AlignResult
import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

case class Strip(diags: IndexedSeq[Diagonal]) extends AnyVal {
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

    /* First row elements */

    var rowScore    = Array.fill(maxWidth + 4)(0)
    val newRowScore = Array.fill(maxWidth + 4)(0)

    /* --- */

    while (i < seqPair.s1.length && curFirstColInd < seqPair.s2.length) {
      newRowScore(0) = -1000000 /* To prevent using non-strip elements */

      var j = curFirstColInd
      while (j <= curLastColInd) {
        val stScore      = scoreTable(seqPair.s1(i))(seqPair.s2(j))
        val leftScore    = newRowScore.last
        val topScore     = rowScore(j - curFirstColInd + 2)
        val topLeftScore = rowScore(j - curFirstColInd + 1)

        val score = math.max(
          math.max(leftScore + gapPenalty, topScore + gapPenalty),
          topLeftScore + stScore
        )

        if (i == seqPair.s1.length - 1 || j == seqPair.s2.length - 1) {
          maxScore = math.max(score, maxScore)
        }

        newRowScore(j - curFirstColInd + 1) = score
        j += 1
      }

      newRowScore(curLastColInd - curFirstColInd + 2) = -1000000 /* To prevent using non-strip elements */
      newRowScore(curLastColInd - curFirstColInd + 3) = -1000000
      rowScore = newRowScore

      cnt += 1
      i   += 1

      curFirstColInd = math.max(-bottomBound + cnt, 0)
      curLastColInd  = math.min(math.max(-topBound + cnt, 0), seqPair.s2.length - 1)
    }

    AlignResult(maxScore, None)
  }

  def smithWatermanScoreAlign(gapPenalty: Int)
                             (seqPair: SeqPair, scoreTable: KeyMatrix): AlignResult = {

    val topBound    = rightOffset
    val bottomBound = leftOffset
    val maxWidth    = leftOffset - rightOffset

    var cnt = 0
    var i = math.max(topBound, 0)

    var curFirstColInd = math.max(-bottomBound, 0)
    var curLastColInd  = math.max(-topBound,    0)

    var maxScore    = 0
    var maxScoreRow = -1
    var maxScoreCol = -1
    val prevMap     = collection.mutable.Map[(Int, Int), (Int, Int)]()

    /* First row elements */

    var rowScore    = Array.fill(maxWidth + 4)(0)
    val newRowScore = Array.fill(maxWidth + 4)(0)

    /* --- */

    while (i < seqPair.s1.length && curFirstColInd < seqPair.s2.length) {
      newRowScore(0) = -1000000 /* To prevent using non-strip elements */

      var j = curFirstColInd
      while (j <= curLastColInd) {
        val stScore      = scoreTable(seqPair.s1(i))(seqPair.s2(j))
        val leftScore    = newRowScore.last
        val topScore     = rowScore(j - curFirstColInd + 2)
        val topLeftScore = rowScore(j - curFirstColInd + 1)

        val score = math.max(
          math.max(leftScore + gapPenalty, topScore + gapPenalty),
          topLeftScore + stScore
        )

        score match {
          case _ if score == leftScore + gapPenalty => prevMap((i, j)) = (i,     j - 1)
          case _ if score == topScore  + gapPenalty => prevMap((i, j)) = (i - 1, j)
          case _                                    => prevMap((i, j)) = (i - 1, j - 1)
        }

        if (i == seqPair.s1.length - 1 || j == seqPair.s2.length - 1) {
          maxScore    = math.max(score, maxScore)
          maxScoreRow = i
          maxScoreCol = j
        }

        newRowScore(j - curFirstColInd + 1) = score
        j += 1
      }

      newRowScore(curLastColInd - curFirstColInd + 2) = -1000000 /* To prevent using non-strip elements */
      newRowScore(curLastColInd - curFirstColInd + 3) = -1000000
      rowScore = newRowScore

      cnt += 1
      i   += 1

      curFirstColInd = math.max(-bottomBound + cnt, 0)
      curLastColInd  = math.min(math.max(-topBound + cnt, 0), seqPair.s2.length - 1)
    }

    val s1Builder = new StringBuilder()
    val s2Builder = new StringBuilder()
    var curRow    = maxScoreRow
    var curCol    = maxScoreCol

    while (prevMap.contains((curRow, curCol))) {
      val pair = prevMap((curRow, curCol))

      if (pair._1 < curRow) {
        s1Builder.append(seqPair.s1(curRow))
      } else {
        s1Builder.append('-')
      }

      if (pair._2 < curCol) {
        s2Builder.append(seqPair.s2(curCol))
      } else {
        s2Builder.append('-')
      }

      curRow = pair._1
      curCol = pair._2
    }

    //s1Builder.append(seqPair.s1(curRow))
    //s2Builder.append(seqPair.s2(curCol))

    AlignResult(maxScore, Some(s1Builder.toString + "\n" + s2Builder.toString))
  }

  def leftOffset: Int = diags.head.offset

  def rightOffset: Int = diags(diags.length - 1).offset
}
