import java.io.FileWriter

import ru.bmstu.bioinformatics.Utils
import ru.bmstu.bioinformatics.database.converted.{Converter, FileOperator}
import ru.bmstu.bioinformatics.dotplot.DotPlot
import ru.bmstu.bioinformatics.scoring.{SubstringMatchMatrix, WeightMatrix}
import ru.bmstu.bioinformatics.database.initial.{FileReader => OldDbReader}

object Application {

  def main(args: Array[String]): Unit = {

    val ssmm = SubstringMatchMatrix(WeightMatrix.readDefault)

    val converted = Converter.convert(OldDbReader.read(Utils.resourceFile("test.fasta")))
    println(FileOperator.defaultFile)
    FileOperator.write(FileOperator.defaultFile)(converted)

    FileOperator.readDefault.foreach(println)
  }
}
