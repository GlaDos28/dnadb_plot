package ru.bmstu.bioinformatics.algo.util

import ru.bmstu.bioinformatics.algo.Preamble._
import ru.bmstu.bioinformatics.algo.input.{ScoreTable, SeqPair}
import ru.bmstu.bioinformatics.algo.output.AlignResult

import scala.collection.mutable.ListBuffer

class Strip private(val diags: List[Diagonal]) {
    def leftOffset:  Int = diags.head
    def rightOffset: Int = diags.last

    def smithWatermanScore(gapPenalty: Int, outTable: Boolean = false)
                          (implicit seqPair: SeqPair, scoreTable: ScoreTable): AlignResult = {
        val topBound    = leftOffset
        val bottomBound = rightOffset

        var cnt            = 0
        var i              = math.max( topBound,    0)
        var curFirstColInd = math.max(-bottomBound, 0)
        var curLastColInd  = math.max(-topBound,    0)

        var maxScore = 0

        var maxScoreLen = 0
        val debugTable  = if (outTable)
            Some(collection.mutable.IndexedSeq.fill(seqPair.s1.length, seqPair.s2.length)((0, false))) else
            None

        /* First row elements */

        var rowScore = ListBuffer[Int]()

        for (_ <- 0 to curLastColInd - curFirstColInd + 2) {
            rowScore += 0
        }

        /* --- */

        while (i < seqPair.s1.length && curFirstColInd < seqPair.s2.length) {
            var newRowScore = ListBuffer[Int]()
            newRowScore += -1000000 /* To prevent using non-strip elements */

            for (j <- curFirstColInd to curLastColInd) {
                val stScore      = scoreTable.get(seqPair.s1(i), seqPair.s2(j))
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

                if (outTable) {
                    maxScoreLen = math.max(score.toString.length, maxScoreLen)
                    debugTable.get(i)(j) = (score, true)
                }

                newRowScore += score
            }

            newRowScore += -1000000 /* To prevent using non-strip elements */
            rowScore = newRowScore

            cnt += 1
            i   += 1
            curFirstColInd =          math.max(-bottomBound + cnt, 0)
            curLastColInd  = math.min(math.max(-topBound    + cnt, 0), seqPair.s2.length - 1)
        }

        val resTableStr = if (outTable)
            Some(("" /: debugTable.get)(_ + _.mkTabbedString(
                maxScoreLen,
                " ",
                el => if (el._2) el._1.toString.colorify(ColorGreen) else el._1.toString,
                _._1.toString) + '\n')) else
            None

        AlignResult(maxScore, resTableStr)
    }
}

object Strip {
    def scanlineDiagsFitStrip(w: Int)(diags: List[Diagonal]): List[Strip] = {
        val sortedDiags = diags.sortWith(_.offset < _.offset)
        val resBuffer   = ListBuffer[Strip]()

        var strip  = Vector[Diagonal](sortedDiags.head)
        var follow = 1

        /* Util functions */

        val fillUntilPossible = () => {
            while (follow < sortedDiags.size && sortedDiags(follow) - strip.head + 1 <= w) {
                strip :+= sortedDiags(follow)
                follow += 1
            }
        }

        val removeUntilEnough = () => strip = strip.dropWhile(strip.last - _ + 1 > w)

        /* --- */

        fillUntilPossible()
        resBuffer += new Strip(strip.toList)

        while (follow < sortedDiags.size) {
            strip :+= sortedDiags(follow)
            follow += 1

            removeUntilEnough()
            fillUntilPossible()

            resBuffer += new Strip(strip.toList)
        }

        resBuffer.toList
    }
}
