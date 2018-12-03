package ru.bmstu.bioinformatics.algo.input

class DotMatrix(val m: Map[(Int, Int), Int]) {
    def get(rowInd: Int, colInd: Int): Int = m.getOrElse((rowInd, colInd), 0)
    def dotIterator: Iterator[((Int, Int), Int)] = m.iterator
}
