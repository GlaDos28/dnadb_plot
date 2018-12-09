package ru.bmstu.bioinformatics.algo.output

case class AlignResult(score: Int) extends AnyVal

object AlignResult {
  def fromStripAligns(aligns: List[AlignResult]): AlignResult = aligns.maxBy(_.score)
}
