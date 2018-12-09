package ru.bmstu.bioinformatics.algo.util

import ru.bmstu.bioinformatics.scoring.SubstringMatchMatrix.SubstringMatchMatrix
import scala.collection.mutable

/** [[DotPlot]] is a map from an index pair (each corresponding to a start of a substring of a given size)
  * to the weight of the match.
  * If substrings do not match, the value in [[DotPlot]] is zero
  */
object DotPlot {

  type DotPlot = Map[(Int, Int), Int]
  //Maps substrings to its positions in the sequence
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

  def apply(m: SubstringMatchMatrix, s1: String, s2: String, size: Int = 2): DotPlot = {
    val init = for {
      i1 <- s1.indices.dropRight(size - 1)
      sub1 = s1.substring(i1, i1 + size)
      i2 <- s2.indices.dropRight(size - 1) if s2.substring(i2, i2 + size) == sub1
    } yield (i1, i2) -> m(sub1)

    init.toMap.withDefaultValue(0)
  }

  /** Compute [[DotPlot]] by given substring maps */
  def apply(m: SubstringMatchMatrix, ssm1: SubstringMap, ssm2: SubstringMap): DotPlot = {
    val b = mutable.Map.empty[(Int, Int), Int]
    val ks = ssm1.keySet.intersect(ssm2.keySet)
    ks.foreach { ss =>
      val score = m(ss)
      for {
        i1 <- ssm1(ss)
        i2 <- ssm2(ss)
      } {
        b.update((i1, i2), score)
      }
    }

    b.toMap.withDefaultValue(0)
  }

  def toString(substrings: Vector[String], size: Int = 2): String = {
    val b = new StringBuilder()
    b.append(substrings(0))

    substrings.drop(1).foreach { s =>
      b.append(s.last)
    }

    b.toString()
  }
}
