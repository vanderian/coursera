package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of two") = forAll { (x: Int, y: Int) =>
    findMin(insert(x, insert(y, empty))) == math.min(x, y)
  }

  property("delete min") = forAll { (x: Int) =>
    isEmpty(deleteMin(insert(x, empty)))
  }

  property("sorted min") = forAll { (heap: H) =>
    def loop(h: H, acc: List[A]): List[A] = {
      if (isEmpty(h)) acc
      else loop(deleteMin(h), findMin(h) :: acc)
    }
    val l = loop(heap, Nil)
    l == l.sorted.reverse
  }

  property("min of meld") = forAll { (x: H, y: H) =>
    val minX = findMin(x)
    val minY = findMin(y)
    findMin(meld(x, y)) == math.min(minX, minY)
  }

  property("insert") = forAll { (l: List[Int]) =>
    val h = (l foldRight empty) (insert)
    if (!isEmpty(h)) findMin(h) == l.min else 0 == l.size
  }

  property("meld empty") = forAll { (h: H) =>
    meld(empty, h) == h && meld(empty, h) == h
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def loop(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else if (findMin(h1) == findMin(h2)) loop(deleteMin(h1), deleteMin(h2))
      else false
    }

    loop(meld(h1, h2), meld(insert(findMin(h1), deleteMin(h1)), h2))
  }

  property("meld keeps all of both") = {
    val h1 = insert(1, insert(2, insert(3, insert(4, empty))))
    val h2 = insert(5, insert(6, insert(7, insert(8, empty))))
    val melded = meld(h2, h1)
    findMin(deleteMin(melded)) == 2
  }

}
