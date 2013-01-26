package pegpuzzle

/**
 * Program to solve the "cracker barrel" triangle peg puzzle
 * 
 * Coordinates (row, col) of Locations in a triangular grid
 * with an edge size of 5.  
 * I opted for starting at one rather than zero.  
 * Because of this, the width of the base and the row number of 
 * the base are the same.
 * 
 * <pre><code>
 * 
 *             1,1
 *           2,1 2,2
 *         3,1 3,2 3,3
 *       4,1 4,2 4,3 4,4
 *     5,1 5,2 5,3 5,4 5,5
 * </code></pre>
 *
 * @author carey
 */
object App {
  
  case class Location(r: Int, c: Int)

  case class Move(source: Location, target: Location) {
    override def toString = "From: " + source + " To: " + target
  }

  // Because we are doing random access lookup on boards
  // it would be more efficient to store them as arrays.
  // For now I am going to stick with lists until I have 
  // the solution working.
  type Board = List[List[Char]]
  type BoardList = List[Board]

  val e = 'e' // empty slot
  val p = 'p' // peg

  /*
   * Turn board counter clockwise until left edge becomes bottom edge.
   * All rotations of a board can be considered identical configurations.
   * 
   * <pre><code>
   * Given this
   * 
   *     p
   *    p p
   *   p p p
   *  p p p e
   * p p p p e
   *
   * Return this
   *     e
   *    e p
   *   p p p
   *  p p p p 
   * p p p p p
   * </code></pre>
   */
  def rotateBoard(board: Board): Board = {
    def rotateBoardReverse(board: Board): Board = {
      val sz = board.size

      def rotateDiag(srcRow: Int, srcCol: Int): List[Char] = {
        if (srcRow > sz) Nil
        else board(srcRow - 1)(srcCol - 1) :: rotateDiag(srcRow+1, srcCol)
      }

      def rotateDiagRows(srcRow: Int, srcCol: Int): Board = {
        if (srcCol > sz) Nil
        else rotateDiag(srcRow, srcCol) :: rotateDiagRows(srcRow+1, srcCol+1) 
      }
      rotateDiagRows(1, 1)
    }
    rotateBoardReverse(board).reverse
  }

  /**
   * Get the content (peg or empty) at a board location
   */
  def getSlotContent(board: Board, loc: Location): Char =
    board(loc.r - 1)(loc.c - 1)

  /**
   * Does this board location contain a peg?
   */
  def occupied(board: Board, loc: Location): Boolean =
    if (getSlotContent(board, loc) == p) true else false

  /**
   * Assume abs(loc1.r - loc2.r) == 2 or zero
   * and    abs(loc1.c - loc2.c) == 2 
   * (Does this need an assertion?)
   * 
   * Return the location of the slot between the two locations
   */
  def between(loc1: Location, loc2: Location): Location = {
    def getOffset(i: Int, j: Int): Int = 
      if (i == j) i else (i min j)+1

    val c = if (loc1.c == loc2.c) loc1.c else (loc1.c min loc2.c) + 1
    Location(getOffset(loc1.r, loc2.r), getOffset(loc1.c, loc2.c))
  }

  /**
   * Confirm that loc is a valid location on board
   */ 
  def isValidLocation(board: Board, loc: Location): Boolean = {
    val l = 0            // lower limit
    val u = board.size+1 // upper limit

    (loc.r > l) && (loc.r < u) && (loc.c > l) && (loc.c < u)
  }

  /*
   * Note: Edgesize is always equivalent to row number and width
   *       of the base. 
   * 
   * emptyLoc is the slot to leave empty.  This routine was written
   * originally to support building the start configurations.
   * We may need a variant to build new boards generated by 
   * applying a move to a board.  At that point emptyLoc should
   * probably become a list.
   */
  def mkrow(edgesize: Int, emptyLoc: Location) : List[Char] = {
    def addRowElements(rowNum: Int, colNum: Int) : List[Char] = {
      if (colNum == rowNum+1) Nil
      else {
        val id = if (Location(rowNum, colNum) == emptyLoc) e else p
        id :: addRowElements(rowNum, colNum + 1)
      }
    }
    addRowElements(edgesize, 1)
  }

  /*
   * Build a board up by row starting at the base (the widest part)
   */
  def mkboard(edgesize: Int, emptyLoc: Location) : Board = {
    def makeReverseBoard(edgesize: Int): Board = 
      if (edgesize == 0) 
        Nil
      else 
        mkrow(edgesize, emptyLoc) :: makeReverseBoard(edgesize - 1)
    makeReverseBoard(edgesize).reverse
  }

  /*
   * Return true if b2 is a rotation of b1
   */
  def compareBoards(b1: Board, b2: Board): Boolean = {
    val b2a = rotateBoard(b2)
    val b2b = rotateBoard(b2a)

    (b1 == b2a)||(b1 == b2b)
  }

  /*
   * Return true if b2 contains a rotation of b1
   */
  def containsRotation(b1: Board, b2: BoardList): Boolean = {
    b2.exists(b => compareBoards(b, b1))
  }

  // construct all possible boards for this edgesize
  def mkAllBoards(edgesize: Int) = {
    val l = for {
      x <- 1 to edgesize;
      y <- 1 to x;
      z = mkboard(edgesize, Location(x, y))
    } yield z
    l.toList
  }

  /*
   * Remove duplicate boards from a list (where duplicates
   * include rotated versions of a board.)
   */
  def uniqueBoards(boards: BoardList): BoardList = {
    def unique(boards: BoardList, accu: BoardList): BoardList = {
      if (boards.isEmpty) accu
      else {
        if ((!(accu.isEmpty)) && containsRotation(boards.head, accu)) {
          unique(boards.tail, accu)
        }
        else {
          unique(boards.tail, boards.head :: accu)
        }
      }
    }
    unique(boards, Nil)
  }

  /*
   * Get all board configurations, eliminate duplicate rotations 
   * and return the list.
   */ 
  def mkBoards(edgesize: Int): BoardList = {
    uniqueBoards(mkAllBoards(edgesize))
  }

  /**
   * Given a location, generate a list of the targets of all 
   * possible moves.  Note: The result list may contain invalid
   * locations.  These can be filtered later.
   */
  def genPotentialDestinations(loc: Location): List[Location] = 
    List(Location(loc.r - 2, loc.c - 2),
         Location(loc.r - 2, loc.c + 2),
         Location(loc.r,     loc.c - 2),
         Location(loc.r,     loc.c + 2),
         Location(loc.r + 2, loc.c - 2),
         Location(loc.r + 2, loc.c + 2))

  def getTargetLocations(board: Board, loc: Location): List[Location] = {

    // ensure target loc is a real location on the board
    // doesn't have a peg in it
    // and the loc between it and the start position does have a peg

    def cullInvalid(it: Location): Boolean = 
      isValidLocation(board, it) &&
      (!(occupied(board, it))) && 
      occupied(board, between(loc, it))

    genPotentialDestinations(loc).filter(cullInvalid)
  }  

  /**
   * Given a position on the board, find locations that
   * <ul>
   *  <li>are unoccupied
   *  <li>are on the board
   *  <li>are two spaces from the location horizontally or diagonally
   *  <li>have an occupied slot between them and the location
   * </ul>
   */    
  def findMoves(board: Board, loc: Location): List[Move] = {
    def locToMove(l: Location): Move =  Move(loc, l)
    getTargetLocations(board, loc).map(locToMove)
  }

  /**
   * Find all solutions to the puzzle represented by "board"
   */ 
  def solveBoard(board: Board): List[Move] = {
    println(findMoves(board, Location(2, 1)))
    Nil
  }

//  def solveBoards(boards: BoardList): List

  def main(args : Array[String]) {
    solveBoard(mkBoards(5).head)
  }

/*
 *  def main(args : Array[String]) {
 *    println(solve(mkBoards(5))
 *    val x = Move(Location(1, 1), Location(3, 2))
 *    println(x)
 *  }
 */

  // half baked, semi automated test
  def testRotate() = {
    val a = List(List(p),List(p,p),List(p,e,p),List(p,e,e,p),List(p,p,p,p,p))
    val b = rotateBoard(a)
    val c = List(a, b)
    val d = c.toSet
    println(c)
    println(d)
    println(compareBoards(a, b))
  }
}

/*
 *  things to try/do
 *  1) I don't think rotational symmetry is enough.  I think reflective 
 *     symmetry is also an issue.  In other words do these have the same
 *     solution?  In a sense, no, since the exact set of moves can't be 
 *     applied.  But to a human being the steps are going to look awfully
 *     similar.
 *     
 *         p            p	
 *        e p	       p e	
 *       p p p	      p p p	
 *      p p p p	     p p p p	
 *     p p p p p    p p p p p
 *     
 *  2) Can you do a rotation using only map and other list operations
 *  3) Can you use fold or filter or a combination to eliminate duplicates 
 *     in mkAllBoards
 *  4) Write unit tests
 *  5) See if the routines that generate lists backwards can be rewritten
 *     to do them forwards from the start
 *  6) Translate to Clojure
 *  7) Translate to Java 8
 *  8) Translate to Java 7
 *  9) Translate to Smalltalk? (Bigger project.  I don't know Smalltalk at all.)
 * 10) See if you can come up with a reduction of a board configuration list
 *     to a single value that is the same for all three rotations of the 
 *     same board.
 * 11) Find out if dups can be filtered out before (or while) building
 *     all permutations 
 * 12) Consider memoization so we can recognize failed paths more rapidly.
       Does this puzzle become intractable as the board size grows?
 * 13) Convert this to parallel code.
 * 14) Consider converting the lists to arrays for direct element access
 * 15) I need to understand the conversions that happen behind the scenes
 *     requiring me, for example, to invoke tolist in mkAllBoards
 */

