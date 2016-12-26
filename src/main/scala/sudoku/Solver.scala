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

    private def hasValue(sq: Square):Boolean = sq.size == 1

    def isComplete: Boolean = squares.forall(_.forall(_.size == 1))

    private def mapRow(op: Square => Square, row: Int, board: Vector[Vector[Square]]) = {
      board(row).map(op)
    }

    private def mapCol(op: Square => Square, col: Int, board: Vector[Vector[Square]]) = {
      board.map(row => {
        val oldVal = row(col)
        row.updated(col, op(oldVal))
      })
    }

    private def mapArea(pos1: Coord, pos2: Coord) (op: Square => Square, board: Vector[Vector[Square]]) = {

      val rows = Math.min(pos1.y, pos2.y) to Math.max(pos1.y, pos2.y)
      val cols = Math.min(pos1.x,pos2.x) to Math.max(pos1.x,pos2.x)

      board.zipWithIndex.map(zipRow => {
        val (row, i) = zipRow
        if(rows contains i){
          val r = row.zipWithIndex
          r.map( col => {
            val (value, j) = col
            if(cols contains j) op(value)
            else value
          })
        }else{
          row
        }
      })
    }

    override def toString: String = {
      squares.map(_.map(x => if(hasValue(x)) s" ${x.head.toString} " else " . ").mkString).mkString("\n")
    }
  }

}

object Do extends App {
  val board = loadSudoku.head
  println(board.toString)
}
