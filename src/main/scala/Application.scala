
import java.util.concurrent.Executors

import monix.eval.Task
import monix.execution.Scheduler
import ru.bmstu.bioinformatics.Utils
import ru.bmstu.bioinformatics.Utils._
import ru.bmstu.bioinformatics.algo.input.SeqPair
import ru.bmstu.bioinformatics.algo.output.AlignResult
import ru.bmstu.bioinformatics.algo.util.DotPlot.SubstringMap
import ru.bmstu.bioinformatics.algo.util.{DiagSum, DotPlot, Strip}
import ru.bmstu.bioinformatics.algo_legacy.SmithWatermanRaw
import ru.bmstu.bioinformatics.database.converted.{Converter, DatabaseOperator}
import ru.bmstu.bioinformatics.database.initial.{FileReader => OldDbReader}
import ru.bmstu.bioinformatics.scoring.SubstringMatchMatrix.SubstringMatchMatrix
import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix
import ru.bmstu.bioinformatics.scoring.{SubstringMatchMatrix, WeightMatrix}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

object Application {

  val gapPenalty: Int = -2 //штраф за гэп
  val diagonalFilter: Int = 10 //число отбираемых диагоналей
  val stripMaxWidth: Int = 7 //максимальная ширина полосы

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

    val parFactor = 16

    implicit val ec: Scheduler = Scheduler(Executors.newFixedThreadPool(parFactor))

    val recordsCount = DatabaseOperator.count()
    val chunk = recordsCount / parFactor
    val timestart = System.currentTimeMillis()

    val res = Task.gatherUnordered ((0 until parFactor).toList.map { i =>
      processDb(seq, weightMatrix, substrings, ssmm, i * chunk, (i + 1) * chunk)
    })

    res.runSyncUnsafe()

    println("time", System.currentTimeMillis() - timestart)

    println(max)
    println(max2)

    DatabaseOperator.close()
    System.exit(0)
  }

  private def processDb(seq: String,
                        weightMatrix: KeyMatrix,
                        substrings: SubstringMap,
                        substringMatchMatrix: SubstringMatchMatrix,
                        from: Int,
                        to: Int)
                       (implicit ec: ExecutionContext): Task[Unit] = {

    Task.deferFuture {
      DatabaseOperator.read(from, to).foreach { case (id, entry) =>
        val dotplot = DotPlot.apply(substringMatchMatrix, entry.substrings, substrings)
        val seqPair = SeqPair(entry.sequence, seq)

        val diagSum       = DiagSum.fromDotMatrix(dotplot, seqPair.s1.length, seqPair.s2.length, 2)
        val bestOffsets   = diagSum.pickMax(diagonalFilter)
        val strips        = Strip.scanlineDiagsFitStrip(stripMaxWidth)(bestOffsets)
        val stripAligns   = strips.map(_.smithWatermanScore(gapPenalty)(seqPair, weightMatrix))
        val alignRes      = AlignResult.fromStripAligns(stripAligns)

        if (id % 10000 == 0) {
          println(id)
        }
      }
    }
  }
}
