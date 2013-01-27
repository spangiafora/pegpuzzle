package pegpuzzle

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PegPuzzleSuite extends FunSuite {
  import App._
  import SlotVal._

  trait TestData {
    val board = canonicalBoard().head

    val a = Location(1, 1)
    val b = Location(2, 1)
    val c = Location(2, 2)
    val d = Location(3, 1)
    val e = Location(3, 2)
    val f = Location(3, 3)
    val g = Location(4, 1)
    val h = Location(4, 2)
    val i = Location(4, 3)
    val j = Location(4, 4)
    val k = Location(5, 1)
    val l = Location(5, 2)
    val n = Location(5, 3)
    val o = Location(5, 4)
    val p = Location(5, 5)

    val bad1 = Location(5, 6)
    val bad2 = Location(6, 5)
    val bad3 = Location(0, 1)
    val bad4 = Location(1, 2)
  }

  test("Occupied") {
    new TestData {
      assert(!(occupied(board, a)))
      assert(occupied(board, b))
      assert(occupied(board, c))
      assert(occupied(board, d))
      assert(occupied(board, e))
      assert(occupied(board, f))
      assert(occupied(board, g))
      assert(occupied(board, h))
      assert(occupied(board, i))
      assert(occupied(board, j))
      assert(occupied(board, k))
      assert(occupied(board, l))
      assert(occupied(board, n))
      assert(occupied(board, o))
      assert(occupied(board, p))
    }
  }

  test("Valid location") {
    new TestData {
      assert(isValidLocation(board, a))
      assert(isValidLocation(board, b))
      assert(isValidLocation(board, c))
      assert(isValidLocation(board, d))
      assert(isValidLocation(board, e))
      assert(isValidLocation(board, f))
      assert(isValidLocation(board, g))
      assert(isValidLocation(board, h))
      assert(isValidLocation(board, i))
      assert(isValidLocation(board, j))
      assert(isValidLocation(board, k))
      assert(isValidLocation(board, l))
      assert(isValidLocation(board, n))
      assert(isValidLocation(board, o))
      assert(isValidLocation(board, p))

      assert(!(isValidLocation(board, bad1)))
      assert(!(isValidLocation(board, bad2)))
      assert(!(isValidLocation(board, bad3)))
      assert(!(isValidLocation(board, bad4)))
    }
  }

  test("Rotate") {
    val a = List(List(e), 
                 List(e, p), 
                 List(p, p, p), 
                 List(p, p, e, p), 
                 List(p, p, p, p, p))

    val b = rotateBoard(a)
    val c = rotateBoard(b)

    assert(b == List(List(p), 
                     List(p, p), 
                     List(p, e, p), 
                     List(p, p, p, p), 
                     List(e, e, p, p, p)))

    assert(c == List(List(p), 
                     List(p, p), 
                     List(p, p, p), 
                     List(p, e, p, e), 
                     List(p, p, p, p, e)))
  }

  test("Between") {
    new TestData {
      val res1 = between(f, a)
      val res2 = between(d, a)
      val res3 = between(e, l)
      val res4 = between(e, n)

      // reverse arg order
      val res5 = between(a, f)
      val res6 = between(a, d)
      val res7 = between(l, e)
      val res8 = between(n, e)

      // case I had a problem with
      val res9 = between(n, k)

      assert(res1 == Location(2, 2))
      assert(res2 == Location(2, 1))
      assert(res3 == Location(4, 2))
      assert(res4 == Location(4, 3))

      assert(res5 == Location(2, 2))
      assert(res6 == Location(2, 1))
      assert(res7 == Location(4, 2))
      assert(res8 == Location(4, 3))

      assert(res9 == Location(5, 2))
    }
  }
}

