package ru.bmstu.bioinformatics.dotplot

import ru.bmstu.bioinformatics.scoring.SubstringMatchMatrix.SubstringMatchMatrix

/** DotPlot is a map from an index pair (each corresponding to a start of a substring) to a weight of the match.
  * If substrings do not match, the value in DotPlot is zero
  */
object DotPlot {

  type DotPlot = Map[(Int, Int), Int]

  def apply(m: SubstringMatchMatrix, s1: String, s2: String, size: Int = 2): DotPlot = {
    val init = for {
      i1 <- s1.indices.dropRight(size - 1)
      sub1 = s1.substring(i1, i1 + size)
      i2 <- s2.indices.dropRight(size - 1) if s2.substring(i2, i2 + size) == s1
    } yield (i1, i2) -> m(sub1)

    init.toMap.withDefaultValue(0)
  }
}
