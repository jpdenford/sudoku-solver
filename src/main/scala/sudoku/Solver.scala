package sudoku
import Solver._

import scala.collection.mutable

object Solver {

  type Square = Set[Int]
  type Board = Vector[Square]

  case class Coord(x: Int, y:Int)

  case class Area(a: Coord, b: Coord){
    def in(c: Coord): Boolean = c.x <= b.x && c.x >= a.x && c.y <= b.y && c.y >= a.y
  }

  val BOARD_SIZE = 9

  def solve(board: Board): Option[Board] = {
    Some(board)
  }

  def changedIndices(a: Board, b: Board): Vector[Int] = {
    a.zip(b).zipWithIndex.filter(x => x._1._1 != x._1._2).map(_._2)
  }

  def set(index: Int, value: Int, board: Board): Board = {
    val pos = getCoord(index)
    val colUpdated = mapCol(_.-(value), getCoord(index).x, board)
    val rowUpdated = mapRow(_.-(value), getCoord(index).y, colUpdated)
    val areaUpdated = mapQuad(_.-(value), getQuadrant(index), rowUpdated)
    val squareUpdated = areaUpdated.updated(index, Set(value))
//    println("setting\n"+stringify(board)+"\n")
    val indecesToUpdate = changedIndices(board, areaUpdated).filter(x => hasValue(areaUpdated(x)))
//    if(indecesToUpdate.nonEmpty) {  // trigger recursive update
//      indecesToUpdate.foldRight(areaUpdated)( (index, board) => {
//        println(stringify(board)+"\n")
//        set(index, board(index).head, board)
//      })
//    } else areaUpdated
    squareUpdated
  }

  def getQuadrant(index: Int): Area = { //TODO fix
    val pos = getCoord(index)
    val quadSize = 3
    val minX = (pos.x / quadSize) * quadSize
    val minY = (pos.y / quadSize) * quadSize
    Area(Coord(minX, minY), Coord(minX + quadSize - 1, minY + quadSize - 1))
  }

  def mapCol(op: Square => Square, n: Int, board: Board): Board = mapBoard(op, i => getCoord(i).x == n, board)

  def mapRow(op: Square => Square, n: Int, board: Board): Board = mapBoard(op, i => getCoord(i).y == n, board)

  def mapQuad(op: Square => Square, quad: Area, board: Board): Board = mapBoard(op, i => quad in getCoord(i), board)

  def mapBoard(op: Square => Square, pred: Int => Boolean, board: Board): Board = {
    var needUpdate: mutable.MutableList[Int] = mutable.MutableList()
    val firstUpdate = board.zipWithIndex.map(e => {
      if(pred(e._2)) op(e._1)
      else e._1
    })
    firstUpdate
  }

  def getIndex(col: Int, row: Int): Int = row * BOARD_SIZE + col

  def getCoord(index: Int): Coord = Coord(index % BOARD_SIZE, index / BOARD_SIZE)

  def stringify(squares: Board): String = {
    squares.grouped(BOARD_SIZE).map(
      _.map(
        x => if(hasValue(x)) s" ${x.head.toString} " else " . "
      ).mkString).mkString("\n")
//    squares.grouped(BOARD_SIZE).map(_.map(x => {
//      val vals = x.toList.sorted.mkString
//      val padding = List.fill(9-vals.length)(" ").mkString
//      s"|${vals+padding}"
//    }).mkString).mkString("\n")
  }

  def hasValue(sq: Square):Boolean = sq.size == 1

}

class UnsolvableException extends RuntimeException

object Do extends App {
  val board = loadSudoku(2)

}