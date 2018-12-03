package ru.bmstu.bioinformatics.algo.util

import ru.bmstu.bioinformatics.algo.Preamble._
import ru.bmstu.bioinformatics.algo.input.DotMatrix

/* Mutable */
class DiagSum(val iLen: Int, val jLen: Int, k: Int, val data: collection.mutable.IndexedSeq[Int]) {

    def this(iLen: Int, jLen: Int, k: Int) = this(iLen, jLen, k, collection.mutable.IndexedSeq.fill(iLen + jLen - 2 * k + 1)(0))

    def minInd: Int = -jLen + k
    def maxInd: Int =  iLen - k

    def getValue(offset: Int): Int  = data(offset - minInd)
    def inc(offset: Int):      Unit = data(offset - minInd) += 1

    def pickMax(amount: Int): List[Diagonal] = {
        data.zipWithIndex
            .sortWith(_._1 > _._1)
            .take(math.min(amount, data.length))
            .map(p => Diagonal(p._2 + minInd))
            .toList
    }

    override def toString: String = {
        val offsets    = minInd to maxInd
        val maxElemLen = math.max(data.maxElemLen, offsets.maxElemLen)

        new StringBuilder()
            .append("I | " ).append(offsets.mkTabbedString(maxElemLen)).append("\n")
            .append("S | " ).append(data.mkTabbedString(maxElemLen))
            .toString
    }
}

object DiagSum {
    def fromDotMatrix(dm: DotMatrix, rowNum: Int, colNum: Int, k: Int): DiagSum = {
        val diagSum = new DiagSum(rowNum, colNum, k)

        for (el <- dm.dotIterator) {
            diagSum.inc(el._1._1 - el._1._2)
        }

        diagSum
    }
}
