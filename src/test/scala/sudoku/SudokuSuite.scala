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
//  val b2 = boards(2)

  test("getter") {
    assert(b1(0) === Set(2))
    assert(b1(11) === Set(6))
  }

  /*test("updating board1") {
    val nb = b1.set(5, Coord(0,0))
    assert(nb.get(Coord(0,0)) === Set(5))
  }

  test("updating board2") {
    val nb = b1.set(5, Coord(5,7))
    assert(nb.get(Coord(5,7)) === Set(5))
  }

  test("setting value updates row"){
    val nb = emptyBoard.set(1, Coord(0,0))
    println(nb)
    assert((1 to 8).map(n => nb.get(Coord(n, 0))) === Vector.fill(8)(Set(2,3,4,5,6,7,8,9)))
  }

  test("setting value updates col"){
    val nb = emptyBoard.set(1, Coord(0,0))
    assert((1 to 8).map(n => nb.get(Coord(0, n))) === Vector.fill(8)(Set(2,3,4,5,6,7,8,9)))
  }

  test("setting value updates quad"){
    //setting value to 1 should remove it from all possibilities in the quad
    val nb = emptyBoard.set(1, Coord(0,0))
    val range = (0 to 2).toList
    val actual:List[Square] = for {
        y <- range
        x <- range
      } yield nb.get(Coord(x, y))
    assert(actual.toList === (Set(1)::List.fill(8)(Set(2,3,4,5,6,7,8,9))) )
  }

  test("first unknown"){
    assert(emptyBoard.firstUnknown() === Coord(0, 0))
  }*/

//  test("index to coord and coord to index") {
//    assert(getCoord())
//  }

//  test("first unknown2"){
//    assert(b2.firstUnknown() === Coord(5, 1))
//  }
}


