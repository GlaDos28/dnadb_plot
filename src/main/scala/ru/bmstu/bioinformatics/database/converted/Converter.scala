package ru.bmstu.bioinformatics.database.converted

import ru.bmstu.bioinformatics.database.initial.{DbEntry => OldDbEntry}
import ru.bmstu.bioinformatics.dotplot.DotPlot

object Converter {

  def convert(oldDb: Iterator[OldDbEntry], size: Int = 2): Iterator[DbEntry] = {
    oldDb.map { case OldDbEntry((name, sequence)) =>
      DbEntry((name, DotPlot.substrings(sequence, size)))
    }
  }
}
