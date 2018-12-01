import ru.bmstu.bioinformatics.dotplot.DotPlot
import ru.bmstu.bioinformatics.scoring.{SubstringMatchMatrix, WeightMatrix}

object Application {

  def main(args: Array[String]): Unit = {

    val sstrwm = SubstringMatchMatrix(WeightMatrix.readDefault)
//    println(sstrwm.size)
//    sstrwm.foreach(println)

    val dp = DotPlot(sstrwm, "AAAA", "AABAA")
    println(dp.size)
    dp.foreach(println)
  }
}
