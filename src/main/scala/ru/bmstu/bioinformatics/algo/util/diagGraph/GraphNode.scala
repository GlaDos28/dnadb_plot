package ru.bmstu.bioinformatics.algo.util.diagGraph

import ru.bmstu.bioinformatics.algo.input.SeqPair

import scala.collection.mutable.ListBuffer

case class GraphNode(ind: Int, seqPairRef: SeqPair, pos: (Int, Int), outEdges: ListBuffer[GraphEdge] = ListBuffer.empty)
