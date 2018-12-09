package ru.bmstu.bioinformatics.algo.util.diagGraph

import ru.bmstu.bioinformatics.algo.input.SeqPair

import scala.collection.mutable.ListBuffer

class GraphNode(val ind: Int, val seqPairRef: SeqPair, val pos: (Int, Int), val outEdges: ListBuffer[GraphEdge] = ListBuffer.empty)
