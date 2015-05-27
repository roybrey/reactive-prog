package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("adding two elements in empty heap and findMin yields smallest of two") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a2, insert(a1, empty))
    val smallest = if (a1 <= a2) a1 else a2
    findMin(h) == smallest
  }

  property("inserting an element and then deleting min from a heap should result in an empty heap") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("finding and deleting minima from a heap should get a sorted seq of elements") = forAll { h: H =>
    toList(h) == toList(h).sorted
  }

  property("finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll { (h1: H, h2: H) =>
      findMin(meld(h1, h2)) == findMin(h1).min(findMin(h2))
    }

  property("deleting the minimum of a two element heap will yield a heap with the bigger of the two elements") =
    forAll { (a1: Int, a2: Int) =>
      val h = insert(a2, insert(a1, empty))
      deleteMin(h) == insert(a1 max a2, empty)
    }

  property("moving the minimum element to another heap and melding should not affect melding original heaps") =
    forAll { (h1: H, h2: H) =>
      val melded = meld(h1, h2)
      val newMelded = meld(deleteMin(h1), insert(findMin(h1), h2))
      toList(melded) == toList(newMelded)
  }

  private def toList(h: H): List[A] = if (!isEmpty(h)) findMin(h) :: toList(deleteMin(h)) else Nil

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    h <- oneOf[H](empty, genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)



}
