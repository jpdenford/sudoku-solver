package sudoku

import Solver._

import scala.annotation.tailrec
import scala.io.StdIn

object Solver {

  /** Represents a sudoku problem with some pre-computed data like size and quadrant size */
  case class Problem(board: Board, quadrantSize: Int, size: Int)

  val BOARD_SIZE = 9

//  Board.loadSudoku()

}


