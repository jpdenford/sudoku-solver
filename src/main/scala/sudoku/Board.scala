package sudoku

import sudoku.Board.Square

import scala.collection.LinearSeqOptimized
import scala.collection.immutable.{LinearSeq, StreamViewLike}

/** Coordinate which is useful for some operations */
case class Coord(x: Int, y: Int)

object Coord {
  def fromIndex(index: Int, boardSize: Int) = Coord(index % boardSize, index / boardSize)
}

/** Area which is useful for some operations */
case class Area(topLeft: Coord, bottomRight: Coord) {
  def contains(c: Coord): Boolean = c.x <= bottomRight.x && c.x >= topLeft.x && c.y <= bottomRight.y && c.y >= topLeft.y
}

//class Board(state: IndexedSeq[Square]) {
//  def solve()
//}


trait Board {
  def solve: Option[Board]

  def isSolved: Boolean

  def toStringPretty: String
}

final case class BoardImpl(values: IndexedSeq[Square]) extends Board {

  val BOARD_SIZE = 9

  override def isSolved: Boolean = true // TODO fix
  /**
    * Takes a board and does a depth first search to find a solution
    * @return
    */
  def solve: Option[BoardImpl] = {
    val nextUnsolvedIndex = this.firstUnknownIndex
    nextUnsolvedIndex match {
      case None => Some(this)
      case Some(index) => {
        // use a view as we only take the first solution to the board
        val possibleValues = this.values(index).view
        val solutions = for {
          number <- possibleValues
          newBoard <- this.setSquare(index, number) // try this value on the current square
          solution <- newBoard.solve // solve the rest
        } yield solution
        solutions.headOption
      }
    }
  }
  /***
    * See if the given square is solved aka. has a single value
    *
    * @param sq
    * @return
    */
  def hasKnownValue(sq: Square): Boolean = sq.size == 1

  /***
    * Get the first index which isn't set or None if board is full
    */
  def firstUnknownIndex: Option[Int] =
    values.zipWithIndex
    .find(x => !hasKnownValue(x._1))
    .flatMap(x => Some(x._2))

  /**
    * Set the value at the given index.
    * Corresponding row, col and quadrant squares will have value removed
    *
    * @param index the index on the board
    * @param value the value to set the given index to
    * @param board the board
    * @return
    */
  def setSquare(index: Int, value: Char): Option[BoardImpl] = {
    //    val quadrantSize = Math.sqrt(board.size).toInt
    mapBoard(_ - value, connectedSquares(index)) // remove the value from the connected squares
      .map(b => b.updated(index, Set(value))) // then set the actual square
  }

  /***
    * Get the quadrant which includes the given coordinate
    *
    * @param pos the coordinate of the square
    * @return Area representing the quadrant
    */
  def getQuadrant(pos: Coord, quadrantSize: Int): Area = {
    val minX = (pos.x / quadrantSize) * quadrantSize
    val minY = (pos.y / quadrantSize) * quadrantSize
    Area(Coord(minX, minY), Coord(minX + quadrantSize - 1, minY + quadrantSize - 1))
  }

  /** *
    * Returns a predicate which tests if a given index i is in
    * the same row, column or quadrant as n (excluding n)
    *
    * @param n square we're interested in
    * @return predicate function
    */
  def connectedSquares(n: Int): Int => Boolean = {
    val quadrantSize = 3 // TODO replace with argument
    val nCoord =  Coord.fromIndex(n, BOARD_SIZE)
    val nQuad = getQuadrant(nCoord, quadrantSize)

    val inColumn: Int => Boolean = i => Coord.fromIndex(i, BOARD_SIZE).x == nCoord.x

    val inRow: Int => Boolean = i => Coord.fromIndex(i, BOARD_SIZE).y == nCoord.y

    val inQuad: Int => Boolean = i => nQuad contains Coord.fromIndex(i, BOARD_SIZE)
    // note we don't want to include the actual index
    (i: Int) => (i != n) && (inColumn(i) || inRow(i) || inQuad(i))
  }

  private def updated(index: Int, value: Square): BoardImpl = BoardImpl(this.values.updated(index, value))

  /** *
    * Map an operation over a board for the valid squares
    * Option result will contain board if operation is valid for all mapped squares
    *
    * @param op    operation to apply to squares
    * @param pred  predicate for applying op to square
    * @return
    */
  def mapBoard(op: Square => Square, pred: Int => Boolean): Option[BoardImpl] = {
    map(this, 0, op, pred)
  }

  private def map(board: BoardImpl, index: Int, op: Square => Square, pred: Int => Boolean): Option[BoardImpl] = {
    if (index >= board.values.size) Some(board)
    else if (!pred(index)) map(board, index + 1, op, pred) // skip
    else {
      val newBoard = board.updated(index, op(board.values(index)))
      if (newBoard.values(index).isEmpty) None // operation was invalid as square is now empty
      else if (newBoard.values(index).size == 1 && board.values(index).size > 1) { // the square is now solved
        val boardUpdated = newBoard.setSquare(index, newBoard.values(index).head)
        if (boardUpdated.isEmpty) None // couldn't continue with update
        else map(boardUpdated.get, index + 1, op, pred) // successful, continue
      }
      else map(newBoard, index + 1, op, pred)
    }
  }

  /** *
    * Print the board out nicely
    * @return
    */
  override def toStringPretty: String = {
    this.values.grouped(BOARD_SIZE)
      .map(row =>
        row.map(
          square => {
            val value = if (square.size == 1) square.head.toString else " "
            s"| $value "
          }).mkString
      ).mkString("\n")
  }

  /***
    * Print a board as a series of numbers with 0 where no value was found
    * @return a string representing the board
    */
  def linePrint(): String = {
    this.values.map( square => {
      if (square.size == 1) square.head.toString else "0"
    }).mkString
  }

  // set the squares on the board to their specified values
  def setupBoard(values: Seq[(Char, Int)]): Option[BoardImpl] = values match {
    case Nil => Some(this)
    case (n, i) :: rest => this.setSquare(i, n).flatMap(_.setupBoard(rest))
  }



//  override def length: Int = values.length
}

object Board {
  type Square = Set[Char] // a square is a set of possible values
  val chars = "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  private val emptyBoard = Vector.fill(9 * 9)(chars.take(9).toSet)
  def loadSudoku(boardChars: Seq[Char]): Option[BoardImpl] = {
    val toUpdate = boardChars.zipWithIndex.filter(_._1 != '0')
    BoardImpl(emptyBoard).setupBoard(toUpdate)
  }
}


