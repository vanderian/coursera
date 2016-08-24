import kmeans.{KMeans, Point}

import scala.collection.GenSeq

object KM extends KMeans
val p1 = new Point(1, 1, 0)
val p2 = new Point(1, -1, 0)
val p3 = new Point(-1, 1, 0)
val p4 = new Point(-1, -1, 0)
val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
val mean1 = new Point(1, 1, 0)
val mean2 = new Point(-1, 1, 0)
val means: GenSeq[Point] = IndexedSeq(mean1, mean2)

val c = points groupBy (KM.findClosest(_, means))
val n = KM.update(c, means)
KM.converged(0.1)(means, n)

