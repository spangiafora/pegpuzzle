package pegpuzzle

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PegPuzzleSuite extends FunSuite {
  import App._
  import SlotVal._

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
    val a = Location(1,1)
    val d = Location(3,1)
    val e = Location(3,2)
    val f = Location(3,3)
    val l = Location(5,2)
    val n = Location(5,4)

    val res1 = between(f, a)
    val res2 = between(d, a)
    val res3 = between(e, l)
    val res4 = between(e, n)

    // reverse arg order
    val res5 = between(a, f)
    val res6 = between(a, d)
    val res7 = between(l, e)
    val res8 = between(n, e)

    assert(res1 == Location(2, 2))
    assert(res2 == Location(2, 1))
    assert(res3 == Location(4, 2))
    assert(res4 == Location(4, 3))

    assert(res5 == Location(2, 2))
    assert(res6 == Location(2, 1))
    assert(res7 == Location(4, 2))
    assert(res8 == Location(4, 3))
  }
}

