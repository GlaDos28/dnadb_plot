package ru.bmstu.bioinformatics.dotplot

import ru.bmstu.bioinformatics.scoring.SubstringMatchMatrix.SubstringMatchMatrix

/** [[DotPlot]] is a map from an index pair (each corresponding to a start of a substring of a given size)
  * to the weight of the match.
  * If substrings do not match, the value in [[DotPlot]] is zero
  */
object DotPlot {

  type DotPlot = Map[(Int, Int), Int]

  def substrings(s: String, size: Int = 2): Vector[String] = {
    val ss = for {
      i <- s.indices.toString().dropRight(size - 1)
    } yield s.substring(i, i + size)
    ss.toVector
  }

  def apply(m: SubstringMatchMatrix, s1: String, s2: String, size: Int = 2): DotPlot = {
    val init = for {
      i1 <- s1.indices.dropRight(size - 1)
      sub1 = s1.substring(i1, i1 + size)
      i2 <- s2.indices.dropRight(size - 1) if s2.substring(i2, i2 + size) == sub1
    } yield (i1, i2) -> m(sub1)

    init.toMap.withDefaultValue(0)
  }

  /** Compute [[DotPlot]] by given substrings */
  def apply(m: SubstringMatchMatrix, ss1: Vector[String], ss2: Vector[String]): DotPlot = {
    val init = for {
      i1 <- ss1.indices
      sub1 = ss1(i1)
      i2 <- ss2.indices if sub1 == ss2(i2)
    } yield (i1, i2) -> m(sub1)

    init.toMap.withDefaultValue(0)
  }
}
