package ru.bmstu.bioinformatics.database.converted

import ru.bmstu.bioinformatics.database.initial.{DbEntry => OldDbEntry}

object Converter {

  def convert(oldDb: Iterator[OldDbEntry], size: Int = 2): Iterator[DbEntry] = {
    oldDb.map { case OldDbEntry((name, sequence)) =>
      val subStrings = for {
        i <- sequence.indices.dropRight(size - 1)
      } yield sequence.substring(i, i + size)
      DbEntry((name, subStrings.toVector))
    }
  }
}
