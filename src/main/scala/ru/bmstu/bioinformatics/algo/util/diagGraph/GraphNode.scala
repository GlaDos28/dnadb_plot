package ru.bmstu.bioinformatics.algo.util.diagGraph

import ru.bmstu.bioinformatics.algo.input.SeqPair

class GraphNode(val ind: Int, val seqPairRef: SeqPair, val pos: (Int, Int), var outEdges: List[GraphEdge] = Nil)
