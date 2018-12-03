package ru.bmstu.bioinformatics.database.converted

import ru.bmstu.bioinformatics.algo.util.DotPlot
import ru.bmstu.bioinformatics.database.initial.{DbEntry => OldDbEntry}

object Converter {

  def convert(oldDb: Iterator[OldDbEntry], size: Int = 2): Iterator[DbEntry] = {
    oldDb.map { case OldDbEntry((name, sequence)) =>
      DbEntry((name, DotPlot.substrings(sequence, size)))
    }
  }
}
