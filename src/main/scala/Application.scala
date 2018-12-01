import ru.bmstu.bioinformatics.Utils
import ru.bmstu.bioinformatics.database.converted.{Converter, FileReader, FileWriter}
import ru.bmstu.bioinformatics.dotplot.DotPlot
import ru.bmstu.bioinformatics.scoring.{SubstringMatchMatrix, WeightMatrix}
import ru.bmstu.bioinformatics.database.initial.{FileReader => OldDbReader}

object Application {

  def main(args: Array[String]): Unit = {

    val ssmm = SubstringMatchMatrix(WeightMatrix.readDefault)

    val converted = Converter.convert(OldDbReader.apply(Utils.resourceFile("test.fasta")))
    println(FileReader.defaultFile)
    FileWriter.write(FileReader.defaultFile)(converted)

    FileReader.default.foreach(println)
  }
}
