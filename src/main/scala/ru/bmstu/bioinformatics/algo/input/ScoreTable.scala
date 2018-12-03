package ru.bmstu.bioinformatics.algo.input

class ScoreTable(val chMap: Map[Char, Int], val t: Array[Array[Int]]) {

    def this(chars: String, t: Array[Array[Int]]) = this(chars.zipWithIndex.toMap, t)

    def get(ch1: Char, ch2: Char): Int = t(chMap(ch1))(chMap(ch2))
}
