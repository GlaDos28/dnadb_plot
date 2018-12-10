
import java.util.concurrent.Executors

import monix.eval.Task
import monix.execution.Scheduler
import ru.bmstu.bioinformatics.Utils
import ru.bmstu.bioinformatics.Utils._
import ru.bmstu.bioinformatics.algo.input.SeqPair
import ru.bmstu.bioinformatics.algo.output.AlignResult
import ru.bmstu.bioinformatics.algo.util.DotPlot.SubstringMap
import ru.bmstu.bioinformatics.algo.util.diagGraph.DiagGraph
import ru.bmstu.bioinformatics.algo.util.{DiagSum, DotPlot, Strip}
import ru.bmstu.bioinformatics.algo_legacy.SmithWatermanRaw
import ru.bmstu.bioinformatics.database.converted.{Converter, DatabaseOperator}
import ru.bmstu.bioinformatics.database.initial.{FileReader => OldDbReader}
import ru.bmstu.bioinformatics.scoring.SubstringMatchMatrix.SubstringMatchMatrix
import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix
import ru.bmstu.bioinformatics.scoring.{SubstringMatchMatrix, WeightMatrix}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

object Application {

  val gapPenalty:     Int = -2 // Штраф за гэп
  val diagonalFilter: Int = 10 // Число отбираемых диагоналей
  val cutoffScore:    Int = 28 // Минимальный score диагонали

  def main(args: Array[String]): Unit = {
    //todo save bin?
    val weightMatrix = WeightMatrix.readDefault

    val seq = "MVHLTPEEKSAVTALWGKVNVDEVGGEALGRLLVVYPWTQRFFESFGDLSTPDAVMGNPKVKAHGKKVLGAFSDGLAHLDNLKGTFATLSELHCDKLHVDPENFRLLGNVLVCVLAHHFGKEFTPPVQAAYQKVVAGVANALAHKYH"

    val substrings = DotPlot.substringsMap(seq)

//    DatabaseOperator.init()
//    val converted = Converter.convert(OldDbReader.read(Utils.resourceURL("uniprot_sprot.fasta")))
//    DatabaseOperator.write(converted)

    val parFactor = 16

    implicit val ec: Scheduler = Scheduler(Executors.newWorkStealingPool(parFactor))

    val recordsCount = DatabaseOperator.count()
    val chunk = recordsCount / parFactor
    val timestart = System.currentTimeMillis()

    val res = Task.gatherUnordered ((0 until parFactor).toList.map { i =>
      processDb(
        seq = seq,
        weightMatrix = weightMatrix,
        substrings = substrings,
        from = i * chunk,
        to = (i + 1) * chunk,
        timestart = timestart
      )
    })

    res.runSyncUnsafe()

    println("Total time:", System.currentTimeMillis() - timestart)

    DatabaseOperator.close()
    System.exit(0)
  }

  private def processDb(seq: String,
                        weightMatrix: KeyMatrix,
                        substrings: SubstringMap,
                        from: Int,
                        to: Int,
                        timestart: Long)
                       (implicit ec: ExecutionContext): Task[Unit] = {

    Task.deferFuture {
      DatabaseOperator.read(from, to).foreach { case (id, entry) =>
        val seqPair = SeqPair(entry.sequence, seq)

        val diagSum = DiagSum.fromSubstringMaps(entry.substrings, substrings, seqPair.s1.length, seqPair.s2.length, 2)
        val bestTrimDiags = diagSum.bestTrim(diagonalFilter, seqPair, weightMatrix)
        var cutDiags = bestTrimDiags.filter(_.getScore(weightMatrix) >= cutoffScore).toVector

        if (cutDiags.isEmpty) {
          cutDiags = Vector(bestTrimDiags.maxBy(_.getScore(weightMatrix)))
        }

        val graphFilteredDiags = DiagGraph.fromDiags(cutDiags, gapPenalty)(weightMatrix).getUsedDiags
        val strip              = new Strip(graphFilteredDiags.toVector.map(_.diag).sortBy(_.offset)(Ordering.Int.reverse))
        val alignRes           = strip.smithWatermanScore(gapPenalty)(seqPair, weightMatrix)

        if (id % 10000 == 0) {
          println(id, alignRes.score, (System.currentTimeMillis() - timestart).toFloat / 1000)
        }
      }
    }
  }
}
