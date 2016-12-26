package sudoku

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Solver._

@RunWith(classOf[JUnitRunner])
class SudokuSuite extends FunSuite  {
  lazy val boards = loadSudoku
  val b1 = boards.head
  test("getter") {
    assert(b1.get(Coord(0,0)) === Set(2))
    assert(b1.get(Coord(0,8)) === Set(5))
  }

  test("updating board") {
    val nb = b1.set(5, Coord(0,0))
    assert(nb.get(Coord(0,0)) === Set(5))
  }


}


