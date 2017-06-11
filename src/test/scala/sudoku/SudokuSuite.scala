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

  test("updating board1") {
    val nb = set(1, 5, b1).get
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


//  test("updating board2") {
//    val nb = set(5, getIndex(Coord(5,7)), b1)
//    assert(nb.get(Coord(5,7)) === Set(5))
//  }

  /* test("setting value updates row"){
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


