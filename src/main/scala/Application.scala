import ru.bmstu.bioinformatics.scoring.{SubstringMatchMatrix, WeightMatrix}

object Application {

  def main(args: Array[String]): Unit = {

    val sstrwm = SubstringMatchMatrix(WeightMatrix.readDefault)
    println(sstrwm.size)
    sstrwm.foreach(println)
  }
}
