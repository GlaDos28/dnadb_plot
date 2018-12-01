import ru.bmstu.bioinformatics.scoring.{SubstringWeightMatrix, WeightMatrix}

object Application {

  def main(args: Array[String]): Unit = {
    println(WeightMatrix.readDefault.keys.map(_._2).toList.sorted)
    println(WeightMatrix.readDefault('H', 'H'))

    val keys = WeightMatrix.readDefault.keys.map(_._1)
    println(SubstringWeightMatrix.allStrings(keys, 1, keys.map(_.toString).toList))
    println(SubstringWeightMatrix.allStrings(keys, 1, keys.map(_.toString).toList).size)
  }
}
