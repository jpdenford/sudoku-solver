package sudoku
import Solver._

object Solver {

  type Square = Set[Int]
//  type Coord = (Int, Int)
  case class Coord(x: Int, y:Int)

  case class Board(squares: Vector[Vector[Square]]) {
    private val size = 9

    def remove(value: Int, position: Coord): Board = {
      if(!((1 to size) contains value)) throw new IllegalArgumentException("Value should be within range")
      //remove the value from the board
      update(_.-(value), position)
    }

    def set(value: Int, position: Coord) = {
      update(_ => Set(value), position)
    }

    private def update(op: Square => Square, pos: Coord): Board = {
      val newSquare = op(squares(pos.y)(pos.x))
      // TODO if newSquare has only one value then remove the value from quadrant, and col/row
      if(newSquare.size == 1){

      }
      val newRow = squares(pos.y).updated(pos.x, newSquare)
      val newTiles = squares.updated(pos.y, newRow)
      Board(newTiles)
    }

    def get(pos: Coord): Square = squares(pos.y)(pos.x)

    def hasValue(pos: Coord): Boolean = hasValue(squares(pos.x)(pos.y))

    def hasValue(sq: Square):Boolean = sq.size == 1

    def isComplete: Boolean = squares.forall(_.forall(_.size == 1))

    override def toString: String = {
      squares.map(_.map(x => if(hasValue(x)) x.head.toString else ".").mkString).mkString("\n")
    }
  }

  def isSafe(value: Int, position: Int, board: Board) = {

  }

}

object Do extends App {
  val board = loadSudoku.head
  println(board.toString)
}
