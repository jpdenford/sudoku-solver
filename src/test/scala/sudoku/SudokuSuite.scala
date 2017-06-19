package sudoku

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Solver._

@RunWith(classOf[JUnitRunner])
class SudokuSuite extends FunSuite  {
  lazy val boards = loadSudoku
  val emptyBoard = boards.head
  val b1 = boards(1)

  test("getter") {
    assert(b1(0) === Set(2))
    assert(b1(11) === Set(6))
  }

  test("updating board1") {
    val nb = setSquare(1, 5, b1).get
    assert(nb(1) === Set(5))
  }

  test("first unknown square") {
    val b = Vector(Set(1), Set(2), Set(3,4))
    assert(firstUnknownIndex(b) === Some(2))
  }

  test("connectedSquares") {
    println("----------\n" + stringify((0 to 80).toVector.map(x => Set(x))))
    assert((0 to 80).filter(connectedSquares(0)).sorted === Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 18, 19, 20, 27, 36, 45, 54, 63, 72))
    assert((0 to 80).filter(connectedSquares(39)).sorted === Vector(3, 12, 21, 30, 31, 32, 36, 37, 38, 39, 40, 41, 42, 43, 44, 48, 49, 50, 57, 66, 75))
  }

}


