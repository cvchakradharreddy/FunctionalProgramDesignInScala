package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty),
    for {
      i <- arbitrary[A]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(i, heap)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("smallest") = forAll { (x: A, y: A) =>
    findMin(insert(x, insert(y, empty))) == {
      if (x < y) x else y
    }
  }

  property("empty") = forAll { (x: A) =>
    deleteMin(insert(x, empty)) == empty
  }

  property("sort") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else {
      def loop(tail: H, min: A): Boolean =
        if (isEmpty(tail)) true
        else if (ord.gteq(findMin(tail), min)) loop(deleteMin(tail), findMin(tail))
        else false

      loop(deleteMin(h), findMin(h))
    }
  }


  property("meld min") = forAll { (x: H, y: H) =>
    val m = meld(x, y)
    if (isEmpty(m)) true
    else {
      val meldMin = findMin(m)
      if (!isEmpty(x) && !isEmpty(y))
        meldMin == findMin(x) || meldMin == findMin(y)
      else if (!isEmpty(x))
        meldMin == findMin(x)
      else if (!isEmpty(y))
        meldMin == findMin(y)
      else false
    }
  }

  property("sort distinct") = forAll(oneOf(const(empty),
    for {
      i <- Gen.oneOf[A](1 to 10)
      heap <- oneOf(const(empty), genHeap)
    } yield insert(i, heap)
  )) { (h: H) =>
    if (isEmpty(h)) true
    else {
      def loop(tail: H, min: A): Boolean =
        if (isEmpty(tail)) true
        else if (ord.gt(findMin(tail), min)) loop(deleteMin(tail), findMin(tail))
        else false

      loop(deleteMin(h), findMin(h))
    }
  }


}
