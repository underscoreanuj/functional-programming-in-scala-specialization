package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- Arbitrary.arbitrary[A]
    m <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert(v, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert arbitrary elements") = forAll {(a: A, b: A) =>
    val heap = insert(a, insert(b, empty))
    findMin(heap) == Math.min(a, b) && findMin(deleteMin(heap)) == Math.max(a, b)
  }

  property("sorted") = forAll { (heap: H) =>
    def isSorted(last_min: Int, heap: H): Boolean = {
    isEmpty(heap) || {
      val min_element = findMin(heap)
      min_element >= last_min && isSorted(min_element, deleteMin(heap))
    }
  }
    isEmpty(heap) || isSorted(findMin(heap), deleteMin(heap))
  }

  property("meld") = forAll { (heap1: H, heap2: H) =>
    def heapEq(heap1: H, heap2: H): Boolean = {
      if (isEmpty(heap1) && isEmpty(heap2)) true
      else {
        val m1 = findMin(heap1)
        val m2 = findMin(heap2)
        m1 == m2 && heapEq(deleteMin(heap1), deleteMin(heap2))
      }
    }

    heapEq(meld(heap1, heap2), meld(deleteMin(heap1), insert(findMin(heap1), heap2)))
  }
}
