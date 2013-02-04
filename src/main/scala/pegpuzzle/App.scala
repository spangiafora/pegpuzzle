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

  type Location = (Int, Int)

  def row(l: Location) = l._1
  def column(l: Location) = l._2

  type Move = (Location, Location)

  def sourcePos(m: Move) = m._1
  def targetPos(m: Move) = m._2

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
  type Row           = List[SlotVal]
  type Board         = List[Row]
  type BoardMove     = (List[Row], Move)
  type BoardList     = List[Board]
  type BoardMoveList = List[BoardMove]
  type Path          = List[Move]
  type SolutionSet   = List[Path]

  def boardRows(b: BoardMove) = b._1
  def lastMove(b: BoardMove) = b._2

  def dummyMove = ((0,0),(0,0))
  /*
   * Turn board counter-clockwise until left edge becomes bottom edge.
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
    board(row(loc) - 1)(column(loc) - 1)  
  }

  /**
   * Does this board location contain a peg?
   */
  def occupied(board: Board, loc: Location): Boolean = {
    getSlotContent(board, loc) == p
  }

  def between(move: Move): Location = {
    between(sourcePos(move), targetPos(move))
  }

  /**
   * Assume abs(row(loc1) - row(loc2)) == 2 or zero
   * and    abs(column(loc1) - column(loc2)) == 2 
   * (Does this need an assertion?)
   * 
   * Return the location of the slot between the two locations
   */
  def between(loc1: Location, loc2: Location): Location = {
    val c = if (column(loc1) == column(loc2)) column(loc1) 
            else (column(loc1) min column(loc2)) + 1

    val r = if (row(loc1) == row(loc2)) row(loc1) 
            else (row(loc1) min row(loc2)) + 1

    (r, c)
  }

  /**
   * Confirm that loc is a valid location on board
   * Remember: the row number is also the width of the row
   */ 
  def isValidLocation(board: Board, loc: Location): Boolean = {
    val l = 0             // lower limit
    val ru = board.size   // upper limit on row
    val cu = row(loc)     // upper limit on col

    (row(loc) > l) && (row(loc) <= ru) && (column(loc) > l) && (column(loc) <= cu)
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
        val id = if ((rowNum, colNum) == emptyLoc) e else p
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
      z = mkBoard(edgesize, (x, y))
    } yield z
    l.toList
  }

  /** construct all possible boards for this edgesize */
  def getAllBoardLocations(edgesize: Int) = {
    val l = for {
      x <- 1 to edgesize;
      y <- 1 to x;
      z = (x, y)
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
    List((row(loc) - 2, column(loc) - 2),
         (row(loc) - 2, column(loc)    ),
         (row(loc),     column(loc) - 2),
         (row(loc),     column(loc) + 2),
         (row(loc) + 2, column(loc)    ),
         (row(loc) + 2, column(loc) + 2))
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
    def locToMove(l: Location): Move =  (loc, l)
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
  def applyMove(board: Board, move: Move): BoardMove = {
    // calculate the new value for a board position
    def slotVal(rowNum: Int, colNum: Int): SlotVal = {
      val loc = (rowNum, colNum)

      if (loc == sourcePos(move)) e 
      else if (loc == targetPos(move)) p
      else if (loc == between(move)) e
      else getSlotContent(board, loc)
    }

    // get values for each slot in a row from left to right
    def upd(row: Row, col: Int, accu: Row): Row =
      if (col > row.size) accu
      else slotVal(row.size, col) :: upd(row, col + 1, accu)

    def updateRow(row: Row): Row = upd(row, 1, Nil)

    // update each of the rows in a board
    val b = board.map(updateRow)
    (b, move)
  }

  /**
   * Apply a list of moves to a board, yielding a list of boards
   */
  def applyMoves(board: Board, moves: Path): BoardMoveList = 
    moves.map(applyMove(board, _:Move))

  /**
   * Find all solutions to the puzzle represented by "board"
   */ 
  def solveBoard(board: BoardMove, accu: Path): Option[SolutionSet] = {
    val moves = findAllMoves(boardRows(board))

    //dumpBoardMove(board)
    //println("Moves: " + moves)
    //println("Path: " + accu)

    if (moves isEmpty) {
      if (pegsOnBoard(boardRows(board)) == 1) Some(List(lastMove(board) :: accu)) 
      else None
    }
    else {
      val boards = applyMoves(boardRows(board), moves)

      Some(solveBoard(boards.head, lastMove(board) :: accu).getOrElse(Nil) 
         ++ solveBoardList(boards.tail, lastMove(board) :: accu))
    } 
  }

  def solveBoardList(boardMoves: BoardMoveList, accu: Path): SolutionSet = {
    // first flatten removes the Options
    // second flatten reduces the list of solution sets 
    // to a single solution set
    boardMoves.map(solveBoard(_:BoardMove, accu)).flatten.flatten
  }

  /** The board most people start with
   * <pre>
   *  <code>
   *  List(List(e), List(p, p), List(p, p, p), List(p, p, p, p), List(p, p, p, p, p))
   * 
   *  or:
   *         e
   *        p p
   *       p p p
   *      p p p p
   *     p p p p p
   *  </code>
   * </pre>
   */
  def canonicalBoard(): Board = mkBoards(5).tail.tail.tail.tail.head

  /** Canonical board plus dummy starting move */
  def canonicalBoardMove(): BoardMoveList = List((mkBoards(5).tail.tail.tail.tail.head, dummyMove))

  def main(args : Array[String]) {
    // get all solutions for the board with five slots on an edge
    // println(solveBoards(mkBoardList(5))) 
    // pretty print the starting board
    // dumpBoard(canonicalBoard())
    // for(path <- (solveBoardList(canonicalBoardMove(), Nil))) dumpMoves(path.reverse)
    test()
  }

  def test() = {
    // I believe this board has one failure path: (3,3) -> (1,1)
    // and at least three success paths:
    // 
    //    (3,3) -> (5,5)
    //    (5,5) -> (5,3)
    //    (5,3) -> (3,3)
    //    (3,3) -> (1,1)
    // and
    //    (3,3) -> (5,3)
    //    (5,3) -> (5,5)
    //    (5,5) -> (3,3)
    //    (3,3) -> (1,1) 
    // and
    //    (3,3) -> (5,3)
    //    (5,3) -> (5,5)
    //    (5,5) -> (3,3)
    //    (2,2) -> (4,4) 

    val x = List((List(List(e), 
                      List(e, p), 
                     List(e, e, p), 
                    List(e, e, p, p), 
                   List(e, e, e, p, e)),  ((0,0),(0,0))))

    val y = solveBoardList(x, Nil)
//    for(p <- y) { println("xxxxx xxxxx xxxxx"); printProgress((boardRows(x head)), p.reverse) }
    dumpBoard(boardRows(x.head))
    for(p <- y) { println("xxxxx xxxxx xxxxx"); dumpMoves(p.reverse) }
  }

  def printProgress(b: Board, m: Path): Unit = {
    if (! (m.isEmpty)) {
      println(m head)
      dumpBoard(b)
      printProgress(boardRows(applyMove(b, m head)), m tail)
    }
  }

  // Pretty print a list
  def dumpMoves(l: Path): Unit = {
    println(">>>")
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

  // Pretty print a board and move
  def dumpBoardMove(b: BoardMove): Unit = {
    println(lastMove(b))
    dumpBoard(boardRows(b))
  }

  def fakeMain(args : Array[String]) {

    // get all solutions for the board with five slots on an edge
    // println(solveBoards(mkBoardList(5))) 

    // solveBoardList(canonicalBoard(), Nil)

    // two partially completed boards for debugging
    val x = List((List(List(p), 
                      List(p, e), 
                     List(e, p, e), 
                    List(p, e, e, p), 
                   List(p, e, e, p, e)),  ((0,0),(0,0))))

    // val x = List((List(List(e), 
    //                   List(e, e), 
    //                  List(e, p, e), 
    //                 List(p, e, e, p), 
    //                List(p, e, e, p, e)),  ((0,0),(0,0))))

    // val x = canonicalBoardMove()

    // pretty print the starting board
    dumpBoard(boardRows(x head))

    val y = solveBoardList(x, Nil)
    // for(p <- y) dumpMoves(p.reverse)

    for(p <- y) { println("xxxxx xxxxx xxxxx"); printProgress((boardRows(x head)), p.reverse) }

    // for(path <- (solveBoardList(x, Nil))) println(path.reverse)
    // for(path <- (solveBoardList(x, Nil))) dumpMoves(path.reverse)
    // for(path <- (solveBoardList(canonicalBoardMove(), Nil))) dumpMoves(path.reverse)
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

