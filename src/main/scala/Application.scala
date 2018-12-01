import ru.bmstu.bioinformatics.scoring.{SubstringWeightMatrix, WeightMatrix}

object Application {

  def main(args: Array[String]): Unit = {

    val sstrwm = SubstringWeightMatrix(WeightMatrix.readDefault)
    println(sstrwm.size)
    sstrwm.foreach(println)
  }
}
