package sudoku

//import sudoku.Board.Square

import scala.collection.{IterableView, LinearSeqOptimized}
import scala.collection.immutable.{LinearSeq, StreamViewLike}

/** Coordinate which is useful for some operations */
case class Coord(x: Int, y: Int)

object Coord {
  def fromIndex(index: Int, boardSize: Int) = Coord(index % boardSize, index / boardSize)
}

/** Area which is useful for some operations */
case class Area(topLeft: Coord, bottomRight: Coord) {
  def contains(c: Coord): Boolean = c.x <= bottomRight.x && c.x >= topLeft.x && c.y <= bottomRight.y && c.y >= topLeft.y
}

case class Square(private val values: Set[Char]) {
  def view: IterableView[Char, Set[Char]] = values.view
  def -(c: Char) = Square(values - c)
  def isSolved: Boolean = values.size == 1
  def exhausted: Boolean = values.isEmpty
  def pickFirst: Char = values.head
  def getSolutionChar: Option[Char] = if(this.isSolved) values.headOption else None
}

object Square {
  def apply(char: Char): Square = Square(Set(char))
}

trait Board {
  def solve: Option[Board]

  def toStringPretty: String
}

final case class BoardImpl(private val values: IndexedSeq[Square]) extends Board {

  val BOARD_SIZE = 9

  /**
    * Takes a board and does a depth first search to find a solution
    * @return
    */
  def solve: Option[BoardImpl] = {
    val nextUnsolvedIndex = this.firstUnknownIndex
    nextUnsolvedIndex match {
      case None => Some(this)
      case Some(index) => {
        // use a view as we only take the first solution to the board
        val possibleValues = this(index).view
        val solutions = for {
          number <- possibleValues
          newBoard <- this.setSquare(index, number) // try this value on the current square
          solution <- newBoard.solve // solve the rest
        } yield solution
        solutions.headOption
      }
    }
  }

  /***
    * Get the first index which isn't set or None if board is full
    */
  def firstUnknownIndex: Option[Int] =
    values.zipWithIndex
    .find(x => !x._1.isSolved)
    .flatMap(x => Some(x._2))

  /**
    * Set the value at the given index.
    * Corresponding row, col and quadrant squares will have value removed
    *
    * @param index the index on the board
    * @param value the value to set the given index to
    * @return
    */
  def setSquare(index: Int, value: Char): Option[BoardImpl] = {
    val pred = connectedIndices(index)
    mapBoard(_ - value, pred) // remove the value from the connected squares
      .map(b => b.updated(index, Square(value))) // then set the actual square
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
    * the same row, column or quadrant as index (excluding index)
    *
    * @param index square we're interested in
    * @return predicate function
    */
  def connectedIndices(index: Int): Int => Boolean = {
    val quadrantSize = 3 // TODO replace with argument
    val nCoord =  Coord.fromIndex(index, BOARD_SIZE)
    val nQuad = getQuadrant(nCoord, quadrantSize)

    val inColumn: Int => Boolean = i => Coord.fromIndex(i, BOARD_SIZE).x == nCoord.x

    val inRow: Int => Boolean = i => Coord.fromIndex(i, BOARD_SIZE).y == nCoord.y

    val inQuad: Int => Boolean = i => nQuad contains Coord.fromIndex(i, BOARD_SIZE)
    (i: Int) => (i != index) && (inColumn(i) || inRow(i) || inQuad(i))
  }

  private def updated(index: Int, newValue: Square): BoardImpl = BoardImpl(this.values.updated(index, newValue))

  def apply(index: Int): Square = this.values(index)
  /** *
    * Map an operation over a board for the valid squares
    * Option result will contain board if operation is valid for all mapped squares
    *
    * @param op    operation to apply to squares
    * @param pred  predicate for applying op to square
    * @return
    */
  def mapBoard(op: Square => Square, pred: Int => Boolean): Option[BoardImpl] = {
    this.map(0, op, pred)
  }

  private def map(index: Int, op: Square => Square, pred: Int => Boolean): Option[BoardImpl] = {
    if (index >= this.values.size) return Some(this)
    val curSquare = this(index)
    if (curSquare.exhausted) None
    else if (!pred(index)) this.map(index + 1, op, pred) // skip
    else {
      val newBoard = this.updated(index, op(curSquare))
      val newSquare = newBoard(index)
      if (newSquare.exhausted) None // operation was invalid as square is now empty
      else if (newSquare.isSolved && !curSquare.isSolved) { // the square is now solved
        val boardUpdated = newBoard.setSquare(index, newSquare.pickFirst)
        boardUpdated match {
          case Some(newB) => newB.map(index + 1, op, pred) // successful, continue
          case None => None // couldn't continue with update
        }
      } else newBoard.map(index + 1, op, pred)
    }
  }

  /**
    * Print the board out nicely
    * @return
    */
  override def toStringPretty: String = {
    this.values.grouped(BOARD_SIZE).map(_.map(
      square => {
        s"${square.getSolutionChar.getOrElse(' ')}"
      }).grouped(3).map(_.mkString).mkString("|")
    ).grouped(3).map(_.mkString("\n")).mkString("\n" + Seq.fill(BOARD_SIZE + 2)("_").mkString + "\n")
  }

  /***
    * Print a board as a series of numbers with 0 where no value was found
    * @return a string representing the board
    */
  override def toString: String = this.values.map(_.getSolutionChar.getOrElse('0')).mkString

  // set the squares on the board to their specified values
  def setupBoard(values: Seq[(Char, Int)]): Option[BoardImpl] = values match {
    case Nil => Some(this)
    case (n, i) :: rest => this.setSquare(i, n).flatMap(_.setupBoard(rest))
  }
}

object BoardImpl {
  val chars = "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  private val emptyBoard = Vector.fill(9 * 9)(Square(chars.take(9).toSet))
  def loadSudoku(boardChars: Seq[Char]): Option[BoardImpl] = {
    val toUpdate = boardChars.zipWithIndex.filter(_._1 != '0')
    BoardImpl(emptyBoard).setupBoard(toUpdate)
  }
}


