package ru.bmstu.bioinformatics.algo

import main.scala.ru.bmstu.bioinformatics.algo.util.{DiagSum, Strip}
import main.scala.ru.bmstu.bioinformatics.algo.input.{DotMatrix, ScoreTable, SeqPair}
import main.scala.ru.bmstu.bioinformatics.algo.output.AlignResult

object cli {
    def main(args: Array[String]): Unit = {

        /* Parse input arguments */

        var params = Map[String, Int]()

        for (arg <- args) {
            val parts = arg.split('=')

            if (parts.length == 2) {
                params += parts(0) -> Integer.parseInt(parts(1))
            }
        }

        val diagonalFilter = params.getOrElse("--diagonalFilter", 3)
        val stripMaxWidth  = params.getOrElse("--stripMaxWidth",  7)
        val gapPenalty     = params.getOrElse("--gapPenalty",    -1)
        val printTable     = params.getOrElse("--printTable",     0) != 0

        /* Additional input */

        val scoreTable = new ScoreTable("ATGC", Array(
            Array( 5, -4, -4, -4),
            Array(-4,  5, -4, -4),
            Array(-4, -4,  5, -4),
            Array(-4, -4, -4,  5)
        ))

        val dm = new DotMatrix(Map(
            (1, 1)  -> 1,
            (2, 2)  -> 1,
            (3, 3)  -> 1,
            (4, 4)  -> 1,
            (6, 5)  -> 1,
            (0, 5)  -> 1,
            (1, 7)  -> 1,
            (2, 8)  -> 1,
            (3, 9)  -> 1,
            (4, 10) -> 1
        ))

        val seqPair = SeqPair(
            "GCATCGGC",
            "CCATCGCCATCG"
        )

        /* Process */

        val diagSum       = DiagSum.fromDotMatrix(dm, seqPair.s1.length, seqPair.s2.length, 2)
        val bestOffsets   = diagSum.pickMax(diagonalFilter)
        val bestDiags     = bestOffsets.map(seqPair.getDiagonalSeqs)
        //val bestTrimDiags = bestDiags.map(_.trimmedToMaxLocal(scoreTable))
        val strips        = Strip.scanlineDiagsFitStrip(stripMaxWidth)(bestOffsets)
        val stripAligns   = strips.map(_.smithWatermanScore(gapPenalty, printTable)(seqPair, scoreTable))
        val alignRes      = AlignResult.fromStripAligns(stripAligns)

        println(alignRes.score)

        if (printTable) {
            println(alignRes.table.get)
        }
    }
}
