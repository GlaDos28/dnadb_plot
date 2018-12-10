package ru.bmstu.bioinformatics.algo.input

import ru.bmstu.bioinformatics.algo.util.Diagonal
import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

case class SeqPair(s1: String, s2: String, pos: (Int, Int) = (0, 0), diag: Diagonal = Diagonal(0)) {

  def getDiagonalSeqs(diag: Diagonal): SeqPair =
    if (diag.offset > 0) {
      SeqPair(
        s1.substring(diag.offset, diag.offset + math.min(s1.length - diag.offset, s2.length)),
        s2.substring(0, math.min(s1.length - diag.offset, s2.length)),
        (diag.offset, 0),
        diag
      )
    } else {
      SeqPair(
        s1.substring(0, math.min(s2.length + diag.offset, s1.length)),
        s2.substring(-diag.offset, -diag.offset + math.min(s2.length + diag.offset, s1.length)),
        (0, -diag.offset),
        diag
      )
    }

  def getScore(scoreTable: KeyMatrix): Int = {
    var score = 0

    for (i <- 0 until minLen) {
      score += scoreTable(s1(i))(s2(i))
    }

    score
  }

  def trimmedToMaxLocal(scoreTable: KeyMatrix): SeqPair = {
    var prevScore = 0
    var firstInds = List(0)
    var lastInd   = 0
    var maxScore  = 0

    for (i <- 0 until minLen) {
      val score = math.max(prevScore + scoreTable(s1(i))(s2(i)), 0)

      if (score == 0) {
        firstInds ::= (i + 1)
      } else if (score > maxScore) {
        maxScore = score
        lastInd = i
      }

      prevScore = score
    }

    val firstInd = firstInds.find(_ <= lastInd).get

    SeqPair(s1.substring(firstInd, lastInd + 1), s2.substring(firstInd, lastInd + 1),
      (pos._1 + firstInd, pos._2 + firstInd), diag)
  }

  lazy val minLen: Int = math.min(s1.length, s2.length)

  override def toString: String = s1 + '\n' + s2
}
