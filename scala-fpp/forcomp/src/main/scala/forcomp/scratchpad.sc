

import scala.collection.SortedMap
import scala.collection.immutable.{List, Nil}

val word = "Hello"

word.groupBy(c => c).map(p => (p._1.toLower, p._2.length)).toList.sortBy(_._1)

val l = List(('b', 2), ('a', 2))
val f = List(('a', 2), ('a', 1))

l :: f
l ++ f