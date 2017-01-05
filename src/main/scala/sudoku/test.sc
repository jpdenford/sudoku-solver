import sudoku.Solver._

import scala.util.matching.Regex

val (x,y) = (1,-1)
List(x, y) forall ((0 until 9-1) contains )


(1 to 10).toSet

val xs = List(List(1),List(2),List(3),List(),List(4)).grouped(3).toList

val commentReg = "#.*".r//  = new Regex("""#.*""") //TOD fix to be startswith #
commentReg.findFirstIn("#123")
commentReg.findFirstIn("123#123")

val b = Vector(
    1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1
).map(x=>Set(x))

val a = Vector(
  2,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,
  1,1,1,2,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1
).map(x => Set(x))

changedIndices(a,b)
a.zipWithIndex.filter(_._1 != Set(1))

List(List(None,Some(5)),List(None,Some(4))).flatten

List().foldRight(1)((a, b) => b+1)

(9 until 1).toList

(0 to 2).toList

List.fill(9)(List.fill(9)(Set(0 to 9)))