package sudoku

import Solver._

import scala.annotation.tailrec
import scala.io.StdIn

object Solver {

  type Square = Set[Char] // a square is a set of possible values
  type Board = Vector[Square]

  /** Represents a sudoku problem with some pre-computed data like size and quadrant size */
  case class Problem(board: Board, quadrantSize: Int, size: Int)

  /** Coordinate which is useful for some operations */
  case class Coord(x: Int, y: Int)

  object Coord {
    def fromIndex(index: Int, boardSize: Int) = Coord(index % boardSize, index / boardSize)
  }
  /** Area which is useful for some operations */
  case class Area(topLeft: Coord, bottomRight: Coord) {
    def contains(c: Coord): Boolean = c.x <= bottomRight.x && c.x >= topLeft.x && c.y <= bottomRight.y && c.y >= topLeft.y
  }

  val BOARD_SIZE = 9

  /**
    * Takes a board and does a depth first search to find a solution
    *
    * @param board to try and solve
    * @return
    */
  def solve(board: Board): Option[Board] = {
    val nextUnsolvedIndex = firstUnknownIndex(board)
    nextUnsolvedIndex match {
      case None => Some(board)
      case Some(index) => {
        // use a view as we only take the first solution to the board
        val possibleValues = board(index).view
        val solutions = for {
          number <- possibleValues
          newBoard <- setSquare(index, number, board) // try this value on the current square
          solution <- solve(newBoard) // solve the rest
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
    *
    * @param board board to search
    * @return
    */
  def firstUnknownIndex(board: Board): Option[Int] = board
    .zipWithIndex
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
  def setSquare(index: Int, value: Char, board: Board): Option[Board] = {
//    val quadrantSize = Math.sqrt(board.size).toInt
    mapBoard(_ - value, connectedSquares(index), board) // remove the value from the connected squares
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

  /** *
    * Map an operation over a board for the valid squares
    * Option result will contain board if operation is valid for all mapped squares
    *
    * @param op    operation to apply to squares
    * @param pred  predicate for applying op to square
    * @param board board apply operations to
    * @return
    */
  def mapBoard(op: Square => Square, pred: Int => Boolean, board: Board): Option[Board] = {

    @tailrec
    def map(board: Board, index: Int): Option[Board] = {
      if (index >= board.size) Some(board)
      else if (!pred(index)) map(board, index + 1) // skip
      else {
        val newBoard = board.updated(index, op(board(index)))
        if (newBoard(index).isEmpty) None // operation was invalid as square is now empty
        else if (newBoard(index).size == 1 && board(index).size > 1) { // the square is now solved
          val boardUpdated = setSquare(index, newBoard(index).head, newBoard)
          if (boardUpdated.isEmpty) None // couldn't continue with update
          else map(boardUpdated.get, index + 1) // successful, continue
        }
        else map(newBoard, index + 1)
      }
    }

    map(board, 0)
  }

  /***
    * Print the board out nicely
    * @param board
    * @return
    */
  def prettyPrint(board: Board): String = {
    board.grouped(BOARD_SIZE)
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
    * @param board
    * @return
    */
  def linePrint(board: Board): String = {
    board.map( square => {
        if (square.size == 1) square.head.toString else "0"
      }).mkString
  }

  val chars = "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  private val emptyBoard = Vector.fill(9 * 9)(chars.take(9).toSet)

  // set the squares on the board to their specified values
  def setupBoard(values: Seq[(Char, Int)], board: Board): Option[Board] = values match {
    case Nil => Some(board)
    case (n, i) :: rest => {
      setSquare(i, n, board).flatMap(b => setupBoard(rest, b))
    }
  }

  def loadSudoku(boardChars: Seq[Char]): Option[Board] = {
    val toUpdate = boardChars.zipWithIndex.filter(_._1 != '0')
    setupBoard(toUpdate, emptyBoard)
  }

}


