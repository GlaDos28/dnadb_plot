package ru.bmstu.bioinformatics.algo.util.diagGraph

import ru.bmstu.bioinformatics.algo.input.SeqPair

case class GraphEdge(outInd: Int, weight: Int, diagRef: Option[SeqPair] = None)
