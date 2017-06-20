package sudoku

import Solver._

import scala.annotation.tailrec

object Solver {

  type Square = Set[Int]
  type Board = Vector[Square]

  case class Coord(x: Int, y: Int)

  case class Area(topLeft: Coord, bottomRight: Coord) {
    def contains(c: Coord): Boolean = c.x <= bottomRight.x && c.x >= topLeft.x && c.y <= bottomRight.y && c.y >= topLeft.y
  }

  val BOARD_SIZE = 9

  /** *
    * Check all rows and colums have values 1 through 9
    * TODO should check size of each square since a square may have multiple values
    *
    * @param board board which is checked for validity
    * @return
    */
  def isSolved(board: Board): Boolean = {
    (0 until BOARD_SIZE).forall(i => {
      val emptySet = Set[Int]()
      val desired = (1 to BOARD_SIZE).toSet
      val rowValues = (0 until BOARD_SIZE).foldLeft(emptySet)((set, j) => set + board(j * BOARD_SIZE + i).head)
      val colValues = (0 until BOARD_SIZE).foldLeft(emptySet)((set, j) => set + board(j + i * BOARD_SIZE).head)
      rowValues == desired && colValues == desired
    })
  }

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
          n <- possibleValues
          newBoard <- setSquare(index, n, board) // try this value on the current square
          solution <- solve(newBoard) // solve the rest
        } yield solution
        solutions.headOption
      }
    }
  }

  def isSet(sq: Square): Boolean = sq.size == 1

  /***
    * Get the first index which isn't set
    *
    * @param board board to search
    * @return
    */
  def firstUnknownIndex(board: Board): Option[Int] = board.zipWithIndex.find(x => !isSet(x._1)).flatMap(x => Some(x._2))

  /**
    * Set the value at the given index.
    * Corresponding row, col and quadrant squares will have value removed
    *
    * @param index the index on the board
    * @param value the value to set the given index to
    * @param board the board
    * @return
    */
  def setSquare(index: Int, value: Int, board: Board): Option[Board] = {
    mapBoard(_ - value, connectedSquares(index), board)
      .map(b => b.updated(index, Set(value))) // set the actual square
  }

  /** *
    * Get cartesian coordinate of a square given the index
    *
    * @param index index in 1dimensional board
    * @return
    */
  def getCoordinate(index: Int): Coord = Coord(index % BOARD_SIZE, index / BOARD_SIZE)

  def getQuadrant(pos: Coord): Area = {
    val quadSize = 3
    val minX = (pos.x / quadSize) * quadSize
    val minY = (pos.y / quadSize) * quadSize
    Area(Coord(minX, minY), Coord(minX + quadSize - 1, minY + quadSize - 1))
  }

  /** *
    * Returns a predicate which tests if a given index i is in
    * the same row, column or quadrant as n (excluding n)
    *
    * @param n square we're interested in
    * @return predicate function
    */
  def connectedSquares(n: Int): Int => Boolean = {
    val nCoord = getCoordinate(n)
    val nQuad = getQuadrant(nCoord)

    val inColumn: Int => Boolean = i => getCoordinate(i).x == nCoord.x

    val inRow: Int => Boolean = i => getCoordinate(i).y == nCoord.y

    val inQuad: Int => Boolean = i => nQuad contains getCoordinate(i)
    // note we don't want to include the actual index
    (i: Int) => (i != n) && (inColumn(i) || inRow(i) || inQuad(i))
  }

  /** *
    * Map an operation over a board for the valid squares
    * Option will contain board if operation is valid for all mapped squares
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
        if (newBoard(index).isEmpty) None // operation was invalid as square now empty
        else if (newBoard(index).size == 1 && board(index).size > 1) {
          // now a single value
          val boardUpdated = setSquare(index, newBoard(index).head, newBoard)
          if (boardUpdated.isEmpty) None // couldn't continue with update
          else map(boardUpdated.get, index + 1) // successful, continue
        }
        else map(newBoard, index + 1)
      }
    }

    map(board, 0)
  }

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

}

class UnsolvableException extends RuntimeException

object Do extends App {
  val board = loadSudoku(3)
  println("starting:\n" + prettyPrint(board))
  val startTime = System.currentTimeMillis
  val boardSolution = solve(board)
  val endTime = System.currentTimeMillis
  println("-------------FINSHED---------------")
  val solution = boardSolution.getOrElse(throw new UnsolvableException)
  println(prettyPrint(solution))
  println("Took: " + (endTime - startTime) + " ms")
}

