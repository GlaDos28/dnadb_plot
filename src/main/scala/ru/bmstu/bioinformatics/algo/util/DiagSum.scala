package ru.bmstu.bioinformatics.algo.util

import ru.bmstu.bioinformatics.algo.Preamble._
import ru.bmstu.bioinformatics.algo.util.DotPlot.DotPlot

/* Mutable */
class DiagSum(val iLen: Int, val jLen: Int, k: Int, val data: collection.mutable.IndexedSeq[Int]) {

  def this(iLen: Int, jLen: Int, k: Int) = this(iLen, jLen, k, collection.mutable.IndexedSeq.fill(iLen + jLen - 2 * k + 1)(0))

  def getValue(offset: Int): Int = data(offset - minInd)

  def minInd: Int = -jLen + k

  def inc(offset: Int): Unit = data(offset - minInd) += 1

  def pickMax(amount: Int): List[Diagonal] = {
    data.indices
      .sortWith(data(_) > data(_)).toList
      .take(math.min(amount, data.length))
      .map(i => Diagonal(i + minInd))
  }

  override def toString: String = {
    val offsets = minInd to maxInd
    val maxElemLen = math.max(data.maxElemLen, offsets.maxElemLen)

    new StringBuilder()
      .append("I | ").append(offsets.mkTabbedString(maxElemLen)).append("\n")
      .append("S | ").append(data.mkTabbedString(maxElemLen))
      .toString
  }

  def maxInd: Int = iLen - k
}

object DiagSum {
  def fromDotMatrix(dm: DotPlot, rowNum: Int, colNum: Int, k: Int): DiagSum = {
    val diagSum = new DiagSum(rowNum, colNum, k)

    for (((i, j), _) <- dm.iterator) {
      diagSum.inc(i - j)
    }

    diagSum
  }
}
