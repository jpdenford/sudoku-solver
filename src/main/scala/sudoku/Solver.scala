package sudoku

import Solver._


object Solver {

  type Square = Set[Int]
  type Board = Vector[Square]

  case class Coord(x: Int, y: Int)

  case class Area(a: Coord, b: Coord) {
    def contains(c: Coord): Boolean = c.x <= b.x && c.x >= a.x && c.y <= b.y && c.y >= a.y
  }

  val BOARD_SIZE = 9

  /** *
    * Check all rows and colums have values 1 - 9
    *
    * @param board
    * @return
    */
  def isSolved(board: Board): Boolean = {
    (0 to 8).forall(i => {
      val emptySet = Set[Int]()
      ((0 to 8).foldLeft(emptySet)((set, j) => (set + board(j * 9 + i).head)) diff (1 to 9).toSet).isEmpty &&
        ((0 to 8).foldLeft(emptySet)((set, j) => (set + board(j + i * 9).head)) diff (1 to 9).toSet).isEmpty
    })
  }

  def solve(board: Board): Option[Board] = {
    val nextUnsolvedIndex = firstUnknownIndex(board)
    nextUnsolvedIndex match {
      case None => Some(board)
      case Some(index) => {
        val possibleVals = board(index)
        possibleVals.view.flatMap(n => set(index, n, board)).flatMap(b => solve(b)).headOption
      }
    }
  }

  def hasValue(sq: Square): Boolean = sq.size == 1

  def firstUnknownIndex(board: Board): Option[Int] = board.zipWithIndex.find(x => !hasValue(x._1)).flatMap(x => Some(x._2))

  /**
    * Set the value at the given index.
    * Corresponding row, col and quadrant squares will have value removed
    *
    * @param index the index on the board
    * @param value the value to set the given index to
    * @param board the board
    * @return
    */
  def set(index: Int, value: Int, board: Board): Option[Board] = {
    mapBoard(_ - value, connectedSquares(index), board)
      .map(b => b.updated(index, Set(value))) // set the actual square
  }

  /** *
    * Get a coordinate of a square given the index
    *
    * @param index
    * @return
    */
  def getCoord(index: Int): Coord = Coord(index % BOARD_SIZE, index / BOARD_SIZE)

  def getQuadrant(index: Int): Area = {
    val pos = getCoord(index)
    val quadSize = 3
    val minX = (pos.x / quadSize) * quadSize
    val minY = (pos.y / quadSize) * quadSize
    Area(Coord(minX, minY), Coord(minX + quadSize - 1, minY + quadSize - 1))
  }

  /** *
    * Returns a predicate which tests if a given index i is in
    * the same row, column or quadrant as n (excluding actual index)
    *
    * @param n
    * @return predicate function
    */
  def connectedSquares(n: Int): Int => Boolean = {
    val coordN = getCoord(n)
    val quadN = getQuadrant(n)

    val inColumn: Int => Boolean = i => getCoord(i).x == coordN.x

    val inRow: Int => Boolean = i => getCoord(i).y == coordN.y

    val inQuad: Int => Boolean = i => quadN contains getCoord(i)
    // not including original index n
    (i: Int) => (i != n) && (inColumn(i) || inRow(i) || inQuad(i))
  }

  /** *
    * Map an operation over a board for the valid squares
    * Option will contain board if operation is valid for all mapped squares
    *
    * @param op
    * @param pred predicate for applying op to square
    * @param board
    * @return
    */
  def mapBoard(op: Square => Square, pred: Int => Boolean, board: Board): Option[Board] = {
    def map(board: Board, index: Int): Option[Board] = {
      if (index >= board.size) return Some(board)
      if (!pred(index)) return map(board, index + 1)
      val newBoard = board.updated(index, op(board(index)))
      if (newBoard(index).isEmpty) return None // operation was invalid as square now empty
      else if (newBoard(index).size == 1 && board(index).size != 1) {
        //operation made this square single value
        val boardUpdated = set(index, newBoard(index).head, newBoard)
        if (boardUpdated.isEmpty) return None // couldn't continue with update
        else return map(boardUpdated.get, index + 1) // successful, continue
      }
      return map(newBoard, index + 1)
    }

    map(board, 0)
  }

  def stringify(squares: Board): String = {
    squares.grouped(BOARD_SIZE).map(_.map(x => {
      val vals = if (x.size == 1) x.head.toString else ""
      val padding = List.fill(2 - vals.length)(" ").mkString
      s"| ${vals + padding}"
    }).mkString).mkString("\n")
  }

}

class UnsolvableException extends RuntimeException

object Do extends App {
  val board = loadSudoku(3)
  val start = System.currentTimeMillis()
  println("starting:\n" + stringify(board))
  val solvedBoard = solve(board).get
  val end = System.currentTimeMillis()
  println("-------------SOLVED---------------")
  println(stringify(solvedBoard))
  println("Took: " + (end - start) + " ms")
}

