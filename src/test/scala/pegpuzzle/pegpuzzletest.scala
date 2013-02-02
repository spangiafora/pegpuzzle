package pegpuzzle

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PegPuzzleSuite extends FunSuite {
  import App._
  import SlotVal.e
  import SlotVal.p

  trait TestData {
    val board = canonicalBoard()

    val board1 = List(List(e),
                     List(p, p),
                    List(p, p, p),
                   List(p, p, p, p),
                  List(p, p, p, p, p))

    val board2 = List(List(p),
                     List(e, p),
                    List(e, p, p),
                   List(p, p, p, p),
                  List(p, p, p, p, p))


    val board3 = List(List(e), 
                    List(e, e), 
                   List(p, e, p), 
                  List(p, p, p, p), 
                 List(p, p, p, p, p))

    val board4 = List(List(e), 
                    List(e, e), 
                   List(p, e, p), 
                  List(p, p, p, e), 
                 List(p, p, p, p, e))

    val a = (1, 1)
    val b = (2, 1)
    val c = (2, 2)
    val d = (3, 1)
    val e1 = (3, 2)
    val f = (3, 3)
    val g = (4, 1)
    val h = (4, 2)
    val i = (4, 3)
    val j = (4, 4)
    val k = (5, 1)
    val l = (5, 2)
    val n = (5, 3)
    val o = (5, 4)
    val p1 = (5, 5)

    val bad1 = (5, 6)
    val bad2 = (6, 5)
    val bad3 = (0, 1)
    val bad4 = (1, 2)
  }

  // I changed the struture of locations, moves, etc
  // so this is just to confirm nothing was broken in the
  // rearrangement of the code
  test("References") {
    val l1 = (1, 2)
    val l2 = (2, 3)  
    val l3 = (1, 1)  
    val l4 = (3, 2)  

    val m1 = (l1, l2)
    val b1 = canonicalBoard()
    val b2 = List(List(e), List(p, p), List(p, p, p), List(p, p, p, p), List(p, p, p, p, p))

    assert(b1 == b2)
    assert(row(l1) == 1)
    assert(column(l1) == 2)
    assert(row(l2) == 2)
    assert(column(l2) == 3)
    assert(sourcePos(m1) == l1)
    assert(targetPos(m1) == l2)
    assert(getSlotContent(b1, l3) == e)
    assert(getSlotContent(b1, l4) == p)
    assert(!(occupied(b1, l3)))
    assert(occupied(b1, l4))
  }

  test("Occupied") {
    new TestData {
      assert(!(occupied(board, a)))
      assert(occupied(board, b))
      assert(occupied(board, c))
      assert(occupied(board, d))
      assert(occupied(board, e1))
      assert(occupied(board, f))
      assert(occupied(board, g))
      assert(occupied(board, h))
      assert(occupied(board, i))
      assert(occupied(board, j))
      assert(occupied(board, k))
      assert(occupied(board, l))
      assert(occupied(board, n))
      assert(occupied(board, o))
      assert(occupied(board, p1))
    }
  }

  test("Valid location") {
    new TestData {
      assert(isValidLocation(board, a))
      assert(isValidLocation(board, b))
      assert(isValidLocation(board, c))
      assert(isValidLocation(board, d))
      assert(isValidLocation(board, e1))
      assert(isValidLocation(board, f))
      assert(isValidLocation(board, g))
      assert(isValidLocation(board, h))
      assert(isValidLocation(board, i))
      assert(isValidLocation(board, j))
      assert(isValidLocation(board, k))
      assert(isValidLocation(board, l))
      assert(isValidLocation(board, n))
      assert(isValidLocation(board, o))
      assert(isValidLocation(board, p1))

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
      val res3 = between(e1, l)
      val res4 = between(e1, n)

      // reverse arg order
      val res5 = between(a, f)
      val res6 = between(a, d)
      val res7 = between(l, e1)
      val res8 = between(n, e1)

      // case I had a problem with
      val res9 = between(n, k)

      assert(res1 == (2, 2))
      assert(res2 == (2, 1))
      assert(res3 == (4, 2))
      assert(res4 == (4, 3))

      assert(res5 == (2, 2))
      assert(res6 == (2, 1))
      assert(res7 == (4, 2))
      assert(res8 == (4, 3))

      assert(res9 == (5, 2))
    }
  }

  test("Potential moves") {
    new TestData {
      assert(genPotentialDestinations((5, 5)).contains((3, 3)))
      assert(! getTargetLocations(board2, (5, 5)).contains((3, 3)))
    }
  }

  test("Apply move") {
    new TestData {
      val goodmove = ((3, 1), (1, 1))
      val badmove  = ((5, 5), (3, 3))

      assert(boardRows(applyMove(board1, goodmove)) == board2)
      assert(boardRows(applyMove(board1, badmove)) != board4)
    }
  }
}

