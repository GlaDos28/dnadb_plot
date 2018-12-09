package ru.bmstu.bioinformatics.algo.util.diagGraph

import ru.bmstu.bioinformatics.algo.input.SeqPair

class GraphEdge(val outInd: Int, val weight: Int, val diagRef: Option[SeqPair] = None)
