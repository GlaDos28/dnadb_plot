package ru.bmstu.bioinformatics.algo.input

import main.scala.ru.bmstu.bioinformatics.algo.util.Diagonal

case class SeqPair(s1: String, s2: String) {

    def minLen: Int = math.min(s1.length, s2.length)

    def getDiagonalSeqs(diag: Diagonal): SeqPair = diag.offset match {
        case x if x > 0 => SeqPair(
            s1.substring(x, x + math.min(s1.length - x, s2.length)),
            s2.substring(0, math.min(s1.length - x, s2.length)))
        case x => SeqPair(
            s1.substring(0, math.min(s2.length + x, s1.length)),
            s2.substring(-x, -x + math.min(s2.length + x, s1.length)))
    }

    def trimmedToMaxLocal(scoreTable: ScoreTable): SeqPair = {
        var prevScore = 0
        var firstInds = 0 :: Nil
        var lastInd   = 0
        var maxScore  = 0

        for (i <- 0 until minLen) {
            val score = math.max(prevScore + scoreTable.get(s1(i), s2(i)), 0)

            if (score == 0) {
                firstInds ::= i + 1
            }

            if (score > maxScore) {
                maxScore = score
                lastInd  = i
            }

            prevScore = score
        }

        val firstInd = firstInds.dropWhile(_ > lastInd).head

        SeqPair(s1.substring(firstInd, lastInd + 1), s2.substring(firstInd, lastInd + 1))
    }

    override def toString: String = s1 + '\n' + s2
}
