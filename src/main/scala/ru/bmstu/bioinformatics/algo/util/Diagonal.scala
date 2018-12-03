package ru.bmstu.bioinformatics.algo.util

case class Diagonal(offset: Int)

object Diagonal {
    implicit def toInt(diag: Diagonal): Int = diag.offset
}