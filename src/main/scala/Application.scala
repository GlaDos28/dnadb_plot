import ru.bmstu.bioinformatics.Utils._
import ru.bmstu.bioinformatics.algo.input.SeqPair
import ru.bmstu.bioinformatics.algo.util.diagGraph.DiagGraph
import ru.bmstu.bioinformatics.algo.util.{DiagSum, DotPlot, Strip}
import ru.bmstu.bioinformatics.algo_legacy.SmithWatermanRaw
import ru.bmstu.bioinformatics.database.converted.DatabaseOperator
import ru.bmstu.bioinformatics.scoring.{SubstringMatchMatrix, WeightMatrix}

import scala.concurrent.ExecutionContext.Implicits.global

//    DatabaseOperator.init()
//    val converted = Converter.convert(OldDbReader.read(Utils.resourceFile("uniprot_sprot.fasta")))
//    DatabaseOperator.write(converted)
object Application {

  val gapPenalty:     Int = -2 // Штраф за гэп
  val diagonalFilter: Int = 10 // Число отбираемых диагоналей
  val cutoffScore:    Int = 28 // Минимальный score диагонали

  def main(args: Array[String]): Unit = {
    val weightMatrix = WeightMatrix.readDefault
    val ssmm = SubstringMatchMatrix(WeightMatrix.readDefault)

    val seq = "MVHLTPEEKSAVTALWGKVNVDEVGGEALGRLLVVYPWTQRFFESFGDLSTPDAVMGNPKVKAHGKKVLGAFSDGLAHLDNLKGTFATLSELHCDKLHVDPENFRLLGNVLVCVLAHHFGKEFTPPVQAAYQKVVAGVANALAHKYH"

    val substrings = DotPlot.substringsMap(seq)
    var max: (String, Int) = ("", Int.MinValue)
    var max2: (String, Int) = ("", Int.MinValue)

//    DatabaseOperator.init()
//    val converted = Converter.convert(OldDbReader.read(Utils.resourceURL("uniprot_sprot.fasta")))
//    DatabaseOperator.write(converted)

    val timestart = System.currentTimeMillis()

    DatabaseOperator.read().foreach { case (id, entry) =>
      val dotplot = DotPlot.apply(ssmm, entry.substrings, substrings)
      val seqPair = SeqPair(entry.sequence, seq)

      val diagSum            = DiagSum.fromDotMatrix(dotplot, seqPair.s1.length, seqPair.s2.length, 2)
      val bestOffsets        = diagSum.pickMax(diagonalFilter)
      val bestDiags          = bestOffsets.map(seqPair.getDiagonalSeqs)
      val bestTrimDiags      = bestDiags.map(_.trimmedToMaxLocal(weightMatrix))
      var cutDiags           = bestTrimDiags.filter(_.getScore(weightMatrix) >= cutoffScore)

      if (cutDiags.isEmpty) {
        cutDiags = bestTrimDiags.maxBy(_.getScore(weightMatrix)) :: Nil
      }

      val graphFilteredDiags = DiagGraph.fromDiags(cutDiags, gapPenalty)(weightMatrix).getUsedDiags
      val strip              = new Strip(graphFilteredDiags.toList.map(_.diag).sortBy(-_.offset))
      val alignRes           = strip.smithWatermanScore(gapPenalty)(seqPair, weightMatrix)

      if (id % 10000 == 0) {
        println(id, alignRes.score, (System.currentTimeMillis() - timestart).asInstanceOf[Float] / 1000)
      }

    }.await()

    println("Total time:", System.currentTimeMillis() - timestart)

    println(max)
    println(max2)
  }
}
