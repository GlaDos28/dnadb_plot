package ru.bmstu.bioinformatics.database.converted

case class DbEntry(value: (String, Vector[String])) {

  def name: String = value._1

  def substrings: Vector[String] = value._2
}
