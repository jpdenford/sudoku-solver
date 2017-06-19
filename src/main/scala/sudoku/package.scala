import sudoku.Solver._

package object sudoku {
  val boardsPath = List("sudoku", "sudokus.txt")

  private val emptyBoard = Vector.fill(9 * 9)((1 to 9).toSet)

  def loadSudoku: List[Board] = {
    val boardStream = Option {
      getClass.getResourceAsStream(boardsPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(boardsPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      // set the squares on the board to their specified values
      def setupBoard(values: List[(Int, Int)], board: Board): Board = values match {
        case Nil => board
        case (n, i) :: rest => {
          val updated = setSquare(i, n, board).getOrElse(throw new UnsolvableException)
          setupBoard(rest, updated)
        }
      }

      val commentReg = "#.*"
      val s = io.Source.fromInputStream(boardStream)
      val allLines = s.getLines.filter(l => !(l.isEmpty || l.matches(commentReg)))
      val boardGroups = allLines.grouped(9).map(_.flatten.map(c => Integer.parseInt(c.toString)))
      val boards = for {
        boardLines <- boardGroups
        toUpdate = boardLines.zipWithIndex.filter(_._1 != 0)
        board = setupBoard(toUpdate.toList, emptyBoard)
      } yield board
      boards.toList
    } catch {
      case e: Exception =>
        println("Could not load sudokus: " + e)
        throw e
    } finally {
      boardStream.close()
    }
  }

}
