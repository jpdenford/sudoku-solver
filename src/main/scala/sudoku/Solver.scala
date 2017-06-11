package sudoku
import Solver._


object Solver {

  type Square = Set[Int]
  type Board = Vector[Square]

  case class Coord(x: Int, y:Int)

  case class Area(a: Coord, b: Coord){
    def contains(c: Coord): Boolean = c.x <= b.x && c.x >= a.x && c.y <= b.y && c.y >= a.y
  }

  val BOARD_SIZE = 9

  def isSolved(board: Board): Boolean = {
    (0 to 8).forall(i => {
      val emptySet = Set[Int]()
      ((0 to 8).foldLeft(emptySet)((set, j) => (set + board(j*9+i).head)) diff (1 to 9).toSet).isEmpty &&
        ((0 to 8).foldLeft(emptySet)((set, j) => (set + board(j+i*9).head)) diff (1 to 9).toSet).isEmpty // TODO do this, make sure all numbers are there (not just that it adds up)
    })

  }

  def solve(board: Board): Option[Board] = {
    val nextInd = firstUnknownIndex(board)
//    if(nextInd.isEmpty && isSolved(board)) return Some(board)
    println("Solve:\n"+nextInd+"\n"+stringify(board))
//    println("Solve:\n"+nextInd)
    nextInd match {
      case None => Some(board)
      case Some(index) => {
//        val solutions = for {
//          n <- board(index)
//          val solution = set(index, n, board)
//          if(!solution.isEmpty)
//          sol <- solve(solution.get)
//        } yield sol
//        if(solutions.isEmpty) None
//        else Some(solutions.head)
        val possibleVals = board(index)
        possibleVals.flatMap(n => set(index, n, board)).flatMap(b => solve(b)).headOption
      }
    }
  }

  def firstUnknownIndex(board: Board): Option[Int] = board.zipWithIndex.find(x => !hasValue(x._1)).flatMap(x => Some(x._2))

  def changedIndices(a: Board, b: Board): Vector[Int] = {
    a.zip(b).zipWithIndex.filter(x => x._1._1 != x._1._2).map(_._2)
  }

  /**
    * Set the value at the given index
    * corresponding row, col and quadrant will have value removed
    * Will return
    * @param index the index on the board
    * @param value the value to set the given index to
    * @param board the board
    * @return
    */
  def set(index: Int, value: Int, board: Board): Option[Board] = {
    val newBoard = mapBoard(_.-(value), connectedSquares(index), board, index)
    newBoard.map(b => b.updated(index, Set(value)))
  }

  def getQuadrant(index: Int): Area = {
    val pos = getCoord(index)
    val quadSize = 3
    val minX = (pos.x / quadSize) * quadSize
    val minY = (pos.y / quadSize) * quadSize
    Area(Coord(minX, minY), Coord(minX + quadSize - 1, minY + quadSize - 1))
  }

  def connectedSquares(n: Int): Int => Boolean = {
    val isInCol = column(n)
    val isInRow = row(n)
    val isInQuad = quad(n)
    (i: Int) => (i != n) && (isInCol(i) || isInRow(i) || isInQuad(i))
  }

  def column(n: Int): Int => Boolean = {
    val col = getCoord(n).x
    i => getCoord(i).x == col
  }

  def row(n: Int): Int => Boolean = {
    val row = getCoord(n).y
    i => getCoord(i).y == row
  }

  def quad(n: Int): Int => Boolean = {
    val quad = getQuadrant(n)
    i => quad contains getCoord(i)
  }

  /***
    * Map an operation over a board for the valid squares
    * Option will contain board if operation is valid for all mapped squares
    * @param op
    * @param pred predicate for applying op to square
    * @param board
    * @return
    */
  def mapBoard(op: Square => Square, pred: Int => Boolean, board: Board, workingIndex: Int): Option[Board] = {
    def map(board: Board, index: Int): Option[Board] = {
      if(index >= board.size) return Some(board)
      if(!pred(index)) return map(board, index + 1)   // TODO move workingindex check into the pred builder
      val newBoard = board.updated(index, op(board(index)))
      if(newBoard(index).isEmpty) return None // operation was invalid as square now empty
      else if(newBoard(index).size == 1 && board(index).size != 1){ //operation made this square single value
        val boardUpdated = set(index, newBoard(index).head, newBoard)
        if(boardUpdated.isEmpty) return None // couldn't continue with update
        else return map(boardUpdated.get, index + 1) // successful, continue
      }
      return map(newBoard, index + 1)
    }
    map(board, 0)
  }

  def getIndex(col: Int, row: Int): Int = row * BOARD_SIZE + col

  def getCoord(index: Int): Coord = Coord(index % BOARD_SIZE, index / BOARD_SIZE)

  def stringify(squares: Board): String = {
    squares.grouped(BOARD_SIZE).map(_.map(x => {
      val vals = x.toList.sorted.mkString
      val padding = List.fill(9 - vals.length)(" ").mkString
      s"|${vals + padding}"
    }).mkString).mkString("\n")
  }

  def hasValue(sq: Square):Boolean = sq.size == 1

}

class UnsolvableException extends RuntimeException

object Do extends App {
  val board = loadSudoku(3)
  println("starting:\n"+stringify(board))
  val solvedBoard = solve(board).get
  println("---------------SOLVED-----------------")
  println(stringify(solvedBoard))
}

