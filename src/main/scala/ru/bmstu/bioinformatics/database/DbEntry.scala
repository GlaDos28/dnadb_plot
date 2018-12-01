package ru.bmstu.bioinformatics.database

case class DbEntry(value: (String, String)) extends AnyVal {

  def name: String = value._1

  def sequence: String = value._2
}
