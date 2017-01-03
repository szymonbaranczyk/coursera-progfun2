package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  //findMin is the same after insertion of same element
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  //findMin will return inserted smaller value
  property("gen2") = forAll { (h:H) =>
    val m = findMin(h)
    val decreasedValue = if(m>Int.MinValue) m-1 else m
    findMin(insert(decreasedValue,h)) == decreasedValue
  }
  //after deleteMin findMin is bigger
  property("gen3") = forAll { (h:H) =>
    val m = findMin(h)
    val h2 = deleteMin(h)
    val m2 = if(isEmpty(h2)) Int.MaxValue else findMin(h2)
    m2 >= m
  }
  //findMin is the same after deleting inserted min
  property("gen4") = forAll { (h:H) =>
    val m = findMin(h)
    val decreasedValue = if(m>Int.MinValue) m-1 else m
    val h2 = deleteMin(insert(decreasedValue,h))
    findMin(h2) == m
  }

  property("gen5") = forAll { (h:H) =>
    val m = findMin(h)
    val decreasedValue = if(m>Int.MinValue) m-1 else m
    val h2 = deleteMin(insert(decreasedValue,h))
    findMin(h2) == m
  }

  property("gen6") = forAll { (x1:Int) =>
    val x2 = if (x1 == Int.MaxValue) 0 else x1
    val h1 = insert(x2+1, insert(x2, insert(x2+2, empty)))
    val h2 = deleteMin(h1)
    h2 == insert(x2+2, insert(x2+1, empty))
  }


}
