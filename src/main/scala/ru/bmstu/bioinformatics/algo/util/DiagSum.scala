package ru.bmstu.bioinformatics.algo.util

import ru.bmstu.bioinformatics.algo.input.SeqPair
import ru.bmstu.bioinformatics.algo.util.DotPlot.{DotPlot, SubstringMap}
import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

/* Mutable */
class DiagSum(iLen: Int, jLen: Int, k: Int, data: Array[Int]) {

  def this(iLen: Int, jLen: Int, k: Int) = this(iLen, jLen, k, Array.fill(iLen + jLen - 2 * k + 1)(0))

  def getValue(offset: Int): Int = data(offset - minInd)

  lazy val minInd: Int = -jLen + k
  lazy val maxInd: Int = iLen - k

  def inc(offset: Int): Unit = data(offset - minInd) += 1

  def pickMax(amount: Int): List[Diagonal] = {
    data.indices
      .sortWith(data(_) > data(_))
      .take(math.min(amount, data.length))
      .map(i => new Diagonal(i + minInd))
      .toList
  }

  def bestTrim(amount: Int, seqPair: SeqPair, weightMatrix: KeyMatrix): IndexedSeq[SeqPair] = {
    data.indices
      .sortWith(data(_) > data(_))
      .take(math.min(amount, data.length))
      .map(i => seqPair.getDiagonalSeqs(new Diagonal(i + minInd)).trimmedToMaxLocal(weightMatrix))
  }
}

object DiagSum {
  def fromDotMatrix(dm: DotPlot, rowNum: Int, colNum: Int, k: Int): DiagSum = {
    val diagSum = new DiagSum(rowNum, colNum, k)

    dm.foreach { tup =>
      val i = tup._1._1
      val j = tup._1._2
      diagSum.inc(i - j)
    }

    diagSum
  }

  def fromSubstringMaps(ssm1: SubstringMap, ssm2: SubstringMap, rowNum: Int, colNum: Int, k: Int): DiagSum = {
    val diagSum = new DiagSum(rowNum, colNum, k)

    ssm1.foreach { kv =>
      val ss = kv._1
      val v = kv._2
      if (ssm2.contains(ss)) {
        v.foreach { i1 =>
          ssm2(ss).foreach(i2 =>  diagSum.inc(i1 - i2))
        }
      }
    }

    diagSum
  }
}
