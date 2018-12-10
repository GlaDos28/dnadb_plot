package ru.bmstu.bioinformatics.algo.util.diagGraph

import ru.bmstu.bioinformatics.algo.input.SeqPair
import ru.bmstu.bioinformatics.scoring.WeightMatrix.KeyMatrix

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DiagGraph(nodes: ListBuffer[GraphNode] = ListBuffer.empty) {
    var curSize: Int = 0

    def addNode(seqPairRef: SeqPair, pos: (Int, Int)): GraphNode = {
      val node = GraphNode(curSize, seqPairRef, pos)
      nodes prepend node
      curSize += 1
      node
    }

    def addEdge(in: GraphNode, outInd: Int, weight: Int, diagRef: Option[SeqPair] = None): GraphEdge = {
      val edge = GraphEdge(outInd, weight, diagRef)
      in.outEdges prepend edge
      edge
    }

    def getUsedDiags: mutable.Set[SeqPair] = { /* Dijkstra algorithm with maximal path search */
      val nodesArr = nodes.sortBy(node => math.min(node.pos._1, node.pos._2)).toArray
      val n        = nodesArr.length
      val dist     = Array.fill(n)(-1000000)
      val was      = Array.fill(n)(false)
      val prev     = Array.fill[Option[Int]](n)(None)

      dist(0) = 0

      /* Process iterations */

      (0 until (n - 1)).foreach { _ =>
        var maxInd = 0
        dist.indices.foreach { i =>
          if (!was(i) && (dist(i) > dist(maxInd))) {
            maxInd = i
          }
        }

        was(maxInd) = true

        nodesArr(maxInd).outEdges.foreach { e =>
          if (!was(e.outInd) && dist(e.outInd) < dist(maxInd) + e.weight) {
            dist(e.outInd) = dist(maxInd) + e.weight
            prev(e.outInd) = Some(maxInd)
          }
        }
      }

      /* Recover path */

      val usedDiags = mutable.Set.empty[SeqPair]
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
    def fromDiags(diags: Vector[SeqPair], gapPenalty: Int)(scoreTable: KeyMatrix): DiagGraph = {
      val graph   = new DiagGraph()

      val nodeMap = diags.map { d =>
        val p1 = graph.addNode(d, d.pos)
        val p2 = graph.addNode(d, (d.pos._1 + d.minLen, d.pos._2 + d.minLen))
        d -> (p1, p2)
      }

      nodeMap.foreach { tup =>
        val seqPair  = tup._1
        val nodePair = tup._2

        graph.addEdge(nodePair._1, nodePair._2.ind, seqPair.getScore(scoreTable), Some(seqPair))

        nodeMap.foreach { tup =>
          val otherNodePair = tup._2
          List(
            (nodePair._1, otherNodePair._1),
            (nodePair._1, otherNodePair._2),
            (nodePair._2, otherNodePair._1),
            (nodePair._2, otherNodePair._2)
          ).foreach { pair =>
            if (pair._2.pos._1 >= pair._1.pos._1 && pair._2.pos._2 >= pair._1.pos._2) {
              graph.addEdge(
                pair._1,
                pair._2.ind,
                gapPenalty * ((pair._2.pos._1 - pair._1.pos._1) + (pair._2.pos._2 - pair._1.pos._2))
              )
            }
          }
        }
      }

      graph
    }
}
