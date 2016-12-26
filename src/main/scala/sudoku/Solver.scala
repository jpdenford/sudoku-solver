package sudoku
import Solver._

object Solver {

  type Square = Set[Int]
  type Coord = (Int, Int)
  //case class Coord(x: Int, y:Int)

  case class Board(squares: Vector[Vector[Square]]) {
    private val size = 9

    def remove(value: Int, position: Coord): Board = {
      if(!((1 until size) contains value)) throw new IllegalArgumentException("Value should be within range")
      //remove the value from the board
      this.update(_.-(value),position)
    }

    def set(value: Int, position: Coord) = {
      //TODO move update logic to get fn
    }

    private def update(op: Square => Square, position: Coord): Board = {
      val (x, y) = position
      val newSquare = op(this.squares(y)(x))
      // TODO if newSquare has only one value then remove the value from quadrant, and col/row
      val newRow = this.squares(y).updated(x, newSquare)
      val newTiles = this.squares.updated(y, newRow)
      Board(newTiles)
    }

    def get(x: Int, y: Int): Square = squares(y)(x)

    def hasValue(pos: Coord): Boolean = hasValue(squares(pos._2)(pos._1))

    def hasValue(sq: Square):Boolean = sq.size == 1

    def isComplete: Boolean = squares.forall(_.forall(_.size == 1))

    override def toString: String = {
      squares.map(_.map(x => if(hasValue(x)) x.head.toString else ".").mkString).mkString("\n")
    }
  }

  def isSafe(value: Int, position: Int, board: Board) = {

  }

}

object Do extends App{
  val board = Board(loadSudoku)
  println(board.toString)
}
