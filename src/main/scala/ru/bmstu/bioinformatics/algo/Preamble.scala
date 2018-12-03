package ru.bmstu.bioinformatics.algo

object Preamble {
    val ColorNone  = "\033[0m"
    val ColorGreen = "\033[0;32m"

    implicit class ExtraStringOps(s: String) {
        def colorify(col: String): String = col + s + ColorNone
    }

    implicit class ExtraTraversableOnceOps[T](seq: Seq[T]) {
        def maxElemLen: Int = (0 /: seq.map(_.toString.length))(math.max)

        def mkTabbedString(maxElemLen: Int,
                           separator: String = " ",
                           toStr: T => String = _.toString,
                           pureStr: T => String = _.toString): String = {
            val builder = new StringBuilder()
            val it      = seq.toIterator

            if (!it.hasNext) {
                return ""
            }

            var elem        = it.next
            var elemStr     = toStr(elem)
            var pureElemStr = pureStr(elem)

            pureElemStr(0) match {
                case '-' => builder.append(elemStr).append(" ")
                case _   => builder.append(' ').append(elemStr)
            }

            while (it.hasNext) {
                builder.append(" " * (maxElemLen - pureElemStr.length))
                elem        = it.next()
                elemStr     = toStr(elem)
                pureElemStr = pureStr(elem)

                pureElemStr(0) match {
                    case '-' => builder.append(elemStr).append(" ")
                    case _   => builder.append(' ').append(elemStr)
                }
            }

            builder.toString
        }
    }
}
