import patmat.Huffman
import patmat.Huffman.Leaf

val l = List('a', 'b', 'a', 'a', 'c')

val g = l.groupBy(c => c).values.map(l => (l.head, l.length)).toList

val lfs = g.map(p => Leaf(p._1, p._2)).sortBy(_.weight)

println(Huffman.decodedSecret)
