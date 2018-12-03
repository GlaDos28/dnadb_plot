import ru.bmstu.bioinformatics.Utils
import ru.bmstu.bioinformatics.Utils._
import ru.bmstu.bioinformatics.algo.util.DotPlot
import ru.bmstu.bioinformatics.database.converted.{Converter, DatabaseOperator}
import ru.bmstu.bioinformatics.database.initial.{FileReader => OldDbReader}
import ru.bmstu.bioinformatics.scoring.{SubstringMatchMatrix, WeightMatrix}

import scala.concurrent.ExecutionContext.Implicits.global

object Application {

  def main(args: Array[String]): Unit = {
    val ssmm = SubstringMatchMatrix(WeightMatrix.readDefault)

//    DatabaseOperator.init()

//    val converted = Converter.convert(OldDbReader.read(Utils.resourceFile("uniprot_sprot.fasta")))
//    DatabaseOperator.write(converted)

    val seq =
      """MVHLTPEEKSAVTALWGKVNVDEVGGEALGRLLVVYPWTQRFFESFGDLSTPDA
        |VMGNPKVKAHGKKVLGAFSDGLAHLDNLKGTFATLSELHCDKLHVDPENFRLLG
        |NVLVCVLAHHFGKEFTPPVQAAYQKVVAGVANALAHKYH""".stripMargin

    println(DotPlot.toString(DotPlot.substrings(seq)) == seq)

//    DatabaseOperator.read().foreach(println).await()
  }
}
