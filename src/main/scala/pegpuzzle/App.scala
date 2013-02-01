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
  
  // I use the terms Location and Slot more or less interchangeably.  Typically
  // I use Slot when I am interested in the contents, and Location when I am
  // interested in the position on the board.
  case class Location(r: Int, c: Int) {
    override def toString = "(Row: " + r + ", Col: " + c + ")"
  }

  case class Move(source: Location, target: Location) {
    override def toString = "From: " + source + " To: " + target
  }

  object SlotVal extends Enumeration {
    type SlotVal = Value
    val p = Value    // peg
    val e = Value    // empty
  }
  import SlotVal._

  // Because we are doing random access lookup on boards
  // it would be more efficient to store them as arrays.
  // For now I am going to stick with lists until I have 
  // the solution working.
  type Row = List[SlotVal]
  type Board = List[Row]
  type BoardList = List[Board]
  type Path = List[Move]
  type SolutionSet = List[Path]

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

      def rotateDiag(srcRow: Int, srcCol: Int): Row = {
        if (srcRow > sz) Nil
        else board(srcRow - 1)(srcCol - 1) :: rotateDiag(srcRow + 1, srcCol)
      }

      def rotateDiagRows(srcRow: Int, srcCol: Int): Board = {
        if (srcCol > sz) Nil
        else rotateDiag(srcRow, srcCol) :: rotateDiagRows(srcRow + 1, srcCol + 1) 
      }
      rotateDiagRows(1, 1)
    }
    rotateBoardReverse(board).reverse
  }

  /**
   * Get the content (peg or empty) at a board location
   */
  def getSlotContent(board: Board, loc: Location): SlotVal = {
    board(loc.r - 1)(loc.c - 1)  
  }

  /**
   * Does this board location contain a peg?
   */
  def occupied(board: Board, loc: Location): Boolean = {
    getSlotContent(board, loc) == p
  }

  def between(move: Move): Location = {
    between(move.source, move.target)
  }

  /**
   * Assume abs(loc1.r - loc2.r) == 2 or zero
   * and    abs(loc1.c - loc2.c) == 2 
   * (Does this need an assertion?)
   * 
   * Return the location of the slot between the two locations
   */
  def between(loc1: Location, loc2: Location): Location = {
    val c = if (loc1.c == loc2.c) loc1.c else (loc1.c min loc2.c) + 1
    val r = if (loc1.r == loc2.r) loc1.r else (loc1.r min loc2.r) + 1

    Location(r, c)
  }

  /**
   * Confirm that loc is a valid location on board
   * Remember: the row number is also the width of the row
   */ 
  def isValidLocation(board: Board, loc: Location): Boolean = {
    val l = 0             // lower limit
    val ru = board.size   // upper limit on row
    val cu = loc.r        // upper limit on col

    (loc.r > l) && (loc.r <= ru) && (loc.c > l) && (loc.c <= cu)
  }

  /*
   * Note: Edgesize is always equivalent to row number and width
   *       of the base. 
   * 
   * emptyLoc is the slot to leave empty.  
   */
  def mkrow(edgesize: Int, emptyLoc: Location) : Row = {
    def addRowElements(rowNum: Int, colNum: Int) : Row = {
      if (colNum == rowNum + 1) Nil
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
  def mkBoard(edgesize: Int, emptyLoc: Location) : Board = {
    def makeReverseBoard(edgesize: Int): Board = 
      if (edgesize == 0) Nil
      else mkrow(edgesize, emptyLoc) :: makeReverseBoard(edgesize - 1)
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

  /** construct all possible boards for this edgesize */
  def mkAllBoards(edgesize: Int) = {
    val l = for {
      x <- 1 to edgesize;
      y <- 1 to x;
      z = mkBoard(edgesize, Location(x, y))
    } yield z
    l.toList
  }

  /** construct all possible boards for this edgesize */
  def getAllBoardLocations(edgesize: Int) = {
    val l = for {
      x <- 1 to edgesize;
      y <- 1 to x;
      z = Location(x, y)
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
        else unique(boards.tail, boards.head :: accu)
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
   * locations.
   */
  def genPotentialDestinations(loc: Location): List[Location] = {
    List(Location(loc.r - 2, loc.c - 2),
         Location(loc.r - 2, loc.c    ),
         Location(loc.r,     loc.c - 2),
         Location(loc.r,     loc.c + 2),
         Location(loc.r + 2, loc.c    ),
         Location(loc.r + 2, loc.c + 2))
  }

  /**
   * Count the occupied slots on a board
   */
  def pegsOnBoard(board: Board): Int = {
    def pegsInRow(row: Row): Int = 
      row.foldLeft(0)((i, j) => if (j == p) i + 1 else i)

    board.foldLeft(0)((x, y) => x + pegsInRow(y))
  }

  /**
   * ensure target location ("it") is a real location on the board
   * and doesn't have a peg in it
   * and the start location ("loc") does have a peg in it
   * and the location between them does have a peg
   */
  def getTargetLocations(board: Board, loc: Location): List[Location] = {

    // Return true if target meets criteria
    def cullInvalid(it: Location): Boolean = {

      isValidLocation(board, it) &&
      (!(occupied(board, it)))   && 
      occupied(board, between(loc, it))
    }

    if (!(occupied(board, loc))) Nil
    else genPotentialDestinations(loc).filter(cullInvalid)
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
  def findMoves(board: Board)(loc: Location): Path = {
    def locToMove(l: Location): Move =  Move(loc, l)
    getTargetLocations(board, loc).map(locToMove)
  }

  /**
   * Find all moves from all starting positions on a board
   */
  def findAllMoves(board: Board): Path = {
    val locs = getAllBoardLocations(board.size)
    locs.flatMap(findMoves(board))
  }

  /**
   * Apply a move to a board and return the resulting board
   * After the move:
   * <ul>
   *   <li>Source slot will be empty
   *   <li>Target slot will be occupied
   *   <li>Slot between them will be empty
   * </ul>
   */ 
  def applyMove(board: Board, move: Move): Board = {
    println("Applying move: " + move)
    // calculate the new value for a board position
    def slotVal(rowNum: Int, colNum: Int): SlotVal = {
      val loc = Location(rowNum, colNum)

      if (loc == move.source) e 
      else if (loc == move.target) p
      else if (loc == between(move)) e
      else getSlotContent(board, loc)
    }

    // get values for each slot in a row from left to right
    def upd(row: Row, col: Int, accu: Row): Row =
      if (col > row.size) accu
      else slotVal(row.size, col) :: upd(row, col + 1, accu)

    def updateRow(row: Row): Row = upd(row, 1, Nil)

    // update each of the rows in a board
    board.map(updateRow)
  }

  /**
   * Apply a list of moves to a board, yielding a list of boards
   */
  def applyMoves(board: Board, moves: Path): BoardList = 
    moves.map(applyMove(board, _:Move))

  /**
   * Find all solutions to the puzzle represented by "board"
   */ 
  def solveBoard(board: Board, accu: Path): SolutionSet = {

    def solve(board: Board, accu: Path): SolutionSet = {
      val moves = findAllMoves(board)

      if (moves isEmpty) {
        if (pegsOnBoard(board) == 1) { 
          dumpBoard(board)
          dumpMoves(accu)
          List(accu)
        }
        else Nil
      }
      else {
        val boards = applyMoves(board, moves)
        solveBoardList(boards, accu)
      } 
    }

    val solution = solve(board, Nil)
    solution
  }

  def solveBoardList(boards: BoardList, accu: Path): SolutionSet =
    boards.map(solveBoard(_:Board, accu)).flatten

  def solveBoards(boards: BoardList): SolutionSet = 
    solveBoardList(boards, Nil)

  /** The board most people start with
   * <pre>
   *  <code>
   *         e
   *        p p
   *       p p p
   *      p p p p
   *     p p p p p
   *  </code>
   * </pre>
   */
  def canonicalBoard(): BoardList = List(mkBoards(5).tail.tail.tail.tail.head)

  def main(args : Array[String]) {
    // println(solveBoards(mkBoards(5))) 
    for(ms <- (solveBoards(canonicalBoard()))) dumpMoves(ms)
  }

  // Pretty print a list
  def dumpMoves(l: Path): Unit = {
    println("MoveList")
    for(m <- l) println(m)
  }

  // Pretty print a board
  def dumpBoard(board: Board): Unit = {
    def db(b: Board, i: Int): Unit = {
      if (b isEmpty) return
      else {
        for(x <- 1 to i) print(" ") 
        for (y <- b.head) {
          print(y)
          print(" ")
        }
        println()
         db(b.tail, i - 1)
      }
    }
    db(board, board.size * 2)
  }
}

/*
 *  things to try/do
 *  1) I am not sure rotational symmetry is enough.  I think reflective 
 *     symmetry could be an issue.  In other words do these have the same
 *     solution?  In a sense, no, since the exact set of moves can't be 
 *     applied.  But to a human being the steps are going to look awfully
 *     similar.
 *     
 *         p            p       
 *        e p          p e      
 *       p p p        p p p     
 *      p p p p      p p p p    
 *     p p p p p    p p p p p
 *     
 *  2) Can you do a rotation using only map and other list operations?
 *  3) Can you use fold or filter or a combination to eliminate duplicates 
 *     in mkAllBoards?
 *  4) Configure unit testing framework and write unit tests
 *  5) See if the routines that generate lists backwards can be rewritten
 *     to do them forwards from the start
 *  6) Apply more object oriented approaches to the types
 *  7) Translate to Clojure
 *  8) Translate to Java 8
 *  9) Translate to Java 7
 * 10) Translate to Smalltalk? (Bigger project.  I don't know Smalltalk at all.)
 * 11) See if you can come up with a reduction of a board configuration list
 *     to a single value that is the same for all three rotations of the 
 *     same board.
 * 12) Find out if dups can be filtered out before (or while) building
 *     all permutations 
 * 13) Does this puzzle become intractable as the board size grows?
 *     Consider caching results keyed by board. If a board has been solved
 *     any new solution will be the same.
 * 14) Convert this to parallel code.
 * 15) Consider converting the lists to arrays for direct element access
 * 16) I need to understand the conversions that happen behind the scenes
 *     requiring me, for example, to invoke tolist in mkAllBoards
 * 17) I am currently thinking of a solution as a list of list of moves.
 *     It might make more sense to build a tree of moves so that shared
 *     paths would be explicitly shared.
 */

