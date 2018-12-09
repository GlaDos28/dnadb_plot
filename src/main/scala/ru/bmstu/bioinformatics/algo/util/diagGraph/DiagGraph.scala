package ru.bmstu.bioinformatics.algo.util.diagGraph

import ru.bmstu.bioinformatics.algo.input.SeqPair
import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

import scala.collection.mutable.ListBuffer

class DiagGraph(nodes: ListBuffer[GraphNode] = ListBuffer.empty) {
    var curSize: Int = 0

    def addNode(seqPairRef: SeqPair, pos: (Int, Int)): GraphNode = {
        val node = new GraphNode(curSize, seqPairRef, pos)
        nodes prepend node
        curSize += 1
        node
    }

    def addEdge(in: GraphNode, outInd: Int, weight: Int, diagRef: Option[SeqPair] = None): GraphEdge = {
        val edge = new GraphEdge(outInd, weight, diagRef)
        in.outEdges prepend edge
        edge
    }

    def getUsedDiags: Set[SeqPair] = { /* Dijkstra algorithm with maximal path search */
        val nodesArr = nodes.toArray.sortBy(node => math.min(node.pos._1, node.pos._2))
        val n        = nodesArr.length
        val dist     = Array.fill(n)(-1000000)
        val was      = Array.fill(n)(false)
        val prev     = Array.fill[Option[Int]](n)(None)

        dist(0) = 0

        /* Process iterations */

        for (_ <- 0 until (n - 1)) {
            val maxInd = dist.indices.filterNot(i => was(i)).maxBy(i => dist(i))
            was(maxInd) = true

            for (e <- nodesArr(maxInd).outEdges) {
                if (!was(e.outInd) && dist(e.outInd) < dist(maxInd) + e.weight) {
                    dist(e.outInd) = dist(maxInd) + e.weight
                    prev(e.outInd) = Some(maxInd)
                }
            }
        }

        /* Recover path */

        var usedDiags = Set.empty[SeqPair]
        var curInd    = n - 1

        while (prev(curInd).isDefined) {
            usedDiags += nodesArr(curInd).seqPairRef
            curInd = prev(curInd).get
        }

        usedDiags += nodesArr(curInd).seqPairRef

        /* Return result */

        usedDiags
    }
}

object DiagGraph {
    def fromDiags(diags: List[SeqPair], gapPenalty: Int)(scoreTable: KeyMatrix): DiagGraph = {
        val graph   = new DiagGraph()
        val nodeMap = diags.zip(diags
            .map(d => graph.addNode(d, d.pos)).zip(diags
            .map(d => graph.addNode(d, (d.pos._1 + d.minLen, d.pos._2 + d.minLen))))).toMap

        for ((seqPair, nodePair) <- nodeMap) {
            graph.addEdge(nodePair._1, nodePair._2.ind, seqPair.getScore(scoreTable), Some(seqPair))

            for (otherNodePair <- nodeMap.values) {
                (   (nodePair._1, otherNodePair._1) ::
                    (nodePair._1, otherNodePair._2) ::
                    (nodePair._2, otherNodePair._1) ::
                    (nodePair._2, otherNodePair._2) ::
                    Nil
                ).foreach(pair => graph.addEdge(pair._1, pair._2.ind, gapPenalty *
                    (math.abs(pair._1.pos._1 - pair._2.pos._1) + math.abs(pair._1.pos._2 - pair._2.pos._2))))
            }
        }

        graph
    }
}
