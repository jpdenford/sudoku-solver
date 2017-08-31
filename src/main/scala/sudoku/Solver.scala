package sudoku

import Solver._

import scala.annotation.tailrec
import scala.io.StdIn

object Solver {

  type Square = Set[Int]
  type Board = Vector[Square]

  case class Coord(x: Int, y: Int)

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
          n <- possibleValues
          newBoard <- setSquare(index, n, board) // try this value on the current square
          solution <- solve(newBoard) // solve the rest
        } yield solution
        solutions.headOption
      }
    }
  }

  /***
    * Check if a given square has a single value aka. it has been set
    * @param square
    * @return
    */
  private def isSet(square: Square): Boolean = square.size == 1

  /***
    * Get the first index which isn't set or None if board is full
    *
    * @param board board to search
    * @return
    */
  private def firstUnknownIndex(board: Board): Option[Int] = board.zipWithIndex.find(x => !isSet(x._1)).flatMap(x => Some(x._2))

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
    // we don't want to include the actual index
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
        if (newBoard(index).isEmpty) None // operation was invalid
        else if (newBoard(index).size == 1 && board(index).size > 1) { // now a single value
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

  def linePrint(board: Board): String = {
    board.map( square => {
        if (square.size == 1) square.head.toString else "0"
      }).mkString
  }

}

class UnsolvableException extends RuntimeException

object Do extends App {
  val boardStrings = Iterator.continually(StdIn.readLine())
    .takeWhile(line => line != null && line.nonEmpty)

  val times = boardStrings.foldRight(List[Long]())((line, nums) => {
    val ints = line.map(c => Integer.parseInt(c.toString)).toList
    val startTime = System.currentTimeMillis()
    val board = loadSudoku(ints).flatMap(b => solve(b))
    val endTime = System.currentTimeMillis()
    val time = endTime - startTime
    // println("-------------FINSHED---------------")
    if(board.nonEmpty) println(linePrint(board.get))
    time::nums
  })

  val count = times.length
  val total = times.sum.toDouble

  println(s" $count solved\nAverage time: ${total/count}")
}

