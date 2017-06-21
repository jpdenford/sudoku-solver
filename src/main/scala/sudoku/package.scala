import sudoku.Solver._

package object sudoku {
  val boardsPath = List("sudoku", "sudokus.txt")

  private val emptyBoard = Vector.fill(9 * 9)((1 to 9).toSet)

  // set the squares on the board to their specified values
  def setupBoard(values: Seq[(Int, Int)], board: Board): Option[Board] = values match {
    case Nil => Some(board)
    case (n, i) :: rest => {
      setSquare(i, n, board).flatMap(b => setupBoard(rest, b))
    }
  }

  def loadSudoku(boardInts: Seq[Int]): Option[Board] = {
     val toUpdate = boardInts.zipWithIndex.filter(_._1 != 0)
     setupBoard(toUpdate, emptyBoard)
  }

  def loadSudokus: Stream[Board] = {
    val boardStream = Option {
      getClass.getResourceAsStream(boardsPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(boardsPath)
    } getOrElse {
      sys.error("Could not load word list, sudoku file not found")
    }
    try {

      val commentReg = "#.*"
      val s = io.Source.fromInputStream(boardStream)
      val allLines = s.getLines.filter(l => !(l.isEmpty || l.matches(commentReg)))
      val boardGroups = allLines
        .grouped(9)
        .toStream
        .map(_.flatten.map(c => Integer.parseInt(c.toString))) // join lines and parse

      boardGroups.flatMap(b => loadSudoku(b))
    } catch {
      case e: Exception =>
        println("Could not load sudokus: " + e)
        throw e
    } finally {
      boardStream.close()
    }
  }

}
