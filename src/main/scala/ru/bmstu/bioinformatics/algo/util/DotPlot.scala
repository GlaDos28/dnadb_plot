package ru.bmstu.bioinformatics.algo.util

import ru.bmstu.bioinformatics.scoring.SubstringMatchMatrix.SubstringMatchMatrix
import scala.collection.mutable

/** [[DotPlot]] is a map from an index pair (each corresponding to a start of a substring of a given size)
  * to the weight of the match.
  * If substrings do not match, the value in [[DotPlot]] is zero
  */
object DotPlot {

  type DotPlot = Map[(Int, Int), Int]
  //Maps substrings to their positions in the sequence
  type SubstringMap = Map[String, Set[Int]]

  def substringsMap(s: String, size: Int = 2): SubstringMap = {
    val b = mutable.Map.empty[String, mutable.Set[Int]]
    for {
      i <- s.indices.dropRight(size - 1)
    } {
      val ss = s.substring(i, i + size)
      b.update(ss, b.getOrElse(ss, mutable.Set.empty) += i)
    }
    b.mapValues(_.toSet).toMap
  }

  /** Compute [[DotPlot]] by given substring maps */
  def apply(m: SubstringMatchMatrix, ssm1: SubstringMap, ssm2: SubstringMap): DotPlot = {
    val b = mutable.AnyRefMap.empty[(Int, Int), Int]

    ssm1.foreach { kv =>
      val ss = kv._1
      val v = kv._2
      if (ssm2.contains(ss)) {
        val score = m(ss)
        v.foreach { i1 =>
          ssm2(ss).foreach(i2 => b.update((i1, i2), score))
        }
      }
    }

    b.toMap
  }
}
