package ru.bmstu.bioinformatics.database.converted

import ru.bmstu.bioinformatics.algo.util.DotPlot.SubstringMap

case class DbEntry(value: (String, String, SubstringMap)) extends AnyVal {

  def name: String = value._1

  def sequence: String = value._2

  /** Maps substrings to set of its positions in the sequence*/
  def substrings: SubstringMap = value._3
}
