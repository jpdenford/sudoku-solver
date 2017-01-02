package sudoku
import Solver._

object Solver {

  type Square = Set[Int]
  type Board = Vector[Vector[Square]]
  case class Coord(x: Int, y:Int)

  case class SudokuBoard(squares: Vector[Vector[Square]]) {
    val size = 9

    def remove(value: Int, position: Coord): SudokuBoard = {
      if(!((1 to size) contains value)) throw new IllegalArgumentException("Value should be within range")
      //remove the value from the board
      update(_.-(value), position)
    }

    def set(value: Int, position: Coord): SudokuBoard = {
      if(!((1 to size) contains value)) throw new IllegalArgumentException("Value should be within range")
      update(_ => Set(value), position)
    }

    private def update(op: Square => Square, pos: Coord): SudokuBoard = {
      val newSquare = op(squares(pos.y)(pos.x))
      // TODO if newSquare has only one value then remove the value from quadrant, and col/row
      val updatedBoard =
        if (newSquare.size == 1) { //should remove all conflicting possibilities
          val n = newSquare.head
          //remove number from all values in same row and col
          val updatedRow = mapRow(_.-(n), pos.y, squares)
          val updatedCol = mapCol(_.-(n), pos.x, updatedRow)
          //remove number from all elements in the quadrant
          val quad = getQuadrant(pos)
          mapArea(quad._1, quad._2)(_.-(n), updatedCol)
        } else squares
      //change the actual square
      val newRow = updatedBoard(pos.y).updated(pos.x, newSquare)
      val newTiles = updatedBoard.updated(pos.y, newRow)
      SudokuBoard(newTiles)
    }

    def get(pos: Coord): Square = squares(pos.y)(pos.x)

    def hasValue(pos: Coord): Boolean = hasValue(squares(pos.x)(pos.y))

    private def hasValue(sq: Square):Boolean = sq.size == 1

    def isComplete: Boolean = squares.forall(_.forall(_.size == 1)) //TODO make more efficient with saved num of completed

    def isValid:Boolean = {
      //rows and cols contain all 9 elements
      val rowsOkay = squares.forall(row => (1 to 9).forall(n => row.contains(n)))
      val colsOkay = squares.transpose.forall(row => (1 to 9).forall(n => row.contains(n)))
      //TODO each quadrant contains 1 - 9 also
      rowsOkay && colsOkay
    }

    /**
      * Gives the coordinate of the first square without a value
      * using order left -> right, top -> bottom
      * @return
      */
    def firstUnknown(): Coord = { //TODO make more efficient with saved index
      val zip = zipped()
      val row = zip.find(row => row._1.exists(sq => !hasValue(sq._1))).head
      val col = row._1.find(sq => !hasValue(sq._1)).get
      Coord(col._2, row._2)
    }

    def zipped(): Vector[(Vector[(Square, Int)], Int)] = squares.map(_.zipWithIndex).zipWithIndex

    private def mapRow(op: Square => Square, row: Int, board: Vector[Vector[Square]]) =
      board.updated(row, board(row).map(op))


    private def mapCol(op: Square => Square, col: Int, board: Vector[Vector[Square]]) =
      board.map( row => row.updated(col, op(row(col))) )

    private def getQuadrant(pos: Coord): (Coord, Coord) = {
      val quadSize = 3
      val minX = (pos.x / quadSize) * quadSize
      val minY = (pos.y / quadSize) * quadSize
      (Coord(minX, minY), Coord(minX + quadSize - 1, minY + quadSize - 1))
    }

    def setSquare(int: Int, pos: Coord, board: Board): Board = {

    }

    private def mapArea(pos1: Coord, pos2: Coord) (op: Square => Square, board: Vector[Vector[Square]]) = {
      import Math.{min, max}
      val rows = min(pos1.y, pos2.y) to max(pos1.y, pos2.y)
      val cols = min(pos1.x, pos2.x) to max(pos1.x, pos2.x)

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
//      squares.map(_.map(x => if(hasValue(x)) s" ${x.head.toString} " else " . ").mkString).mkString("\n")
      squares.map(_.map(x => {
        val vals = x.toList.sorted.mkString
        val padding = List.fill(9-vals.length)(" ").mkString
        s"|${vals+padding}"
      }).mkString).mkString("\n")
    }
  }

  def solve(board: SudokuBoard): Option[SudokuBoard] = {
   /* if(board.isComplete){
      if(board.isValid) Some(board)
      else None
    } else {
      val nextCoord = board.firstUnknown() //TODO save iterating twice - combine isComplete with firstUnknown
      val options = board.get(nextCoord)
      if(options.isEmpty) None
      else {
        println(s"$board\n$nextCoord\n$options\n----\n")
        val boards = options.toList.sorted.map(i => {
//          println("trying:" + i)
          solve(board.set(i, nextCoord))
        }).filter(_.nonEmpty) //lazy eval means okay
        if(!boards.isEmpty) boards.head
        else None
      }
    }*/
    if(board.isValid) Some(board)
    else {
      val nextCoord = board.firstUnknown()
      val options = board.get(nextCoord)
      val boards = for {
        n <- options.toList
        b <- solve(board.set(n, nextCoord))
      } yield b
      if(boards.isEmpty) None
      else Some(boards.head)
    }
  }

}

class UnsolvableException extends RuntimeException

object Do extends App {
  val board = loadSudoku(2)
//  println(board.toString)
  println("\n"+solve(board).get)
}
