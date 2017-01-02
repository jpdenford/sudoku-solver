import sudoku.Solver._

import scala.util.matching.Regex

package object sudoku {
  val dictionaryPath = List("sudoku", "sudokus.txt")

  private val emptyBoard = SudokuBoard(Vector.fill(9)(Vector.fill(9)((1 to 9).toSet)))

  def loadSudoku: List[Solver.SudokuBoard] = {
    val wordstream = Option {
      getClass.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      // set the squares on the board to their specified values
      def setupBoard(values: List[(Int, Coord)], board: SudokuBoard): SudokuBoard = values match {
        case Nil => board
        case (n, c)::rest => {
//          println("\n\n"+board)
          setupBoard(rest, board.set(n,c))
        }
      }

      val commentReg = "#.*"
      val s = io.Source.fromInputStream(wordstream)
      val allLines = s.getLines.filter(l => !(l.isEmpty || l.matches(commentReg))).toVector
      val linesByBoard = allLines.grouped(9).toList
      val boards = linesByBoard.map { b =>
        val boardLines = b.map(
          line => {
            val ns = line.toList
            //split string into chars
            val opts = ns.map({
              case '0' => None
              case n => Some(Integer.parseInt(n.toString))
            })
            opts
          })
        val zippedBoard = boardLines.map(_.zipWithIndex).zipWithIndex
        val values = for {
          row <- zippedBoard
          rowCol <- row._1
          elem <- rowCol._1
        } yield (elem,Coord(rowCol._2, row._2))
        val board = setupBoard(values.toList, emptyBoard)
        println("--BOARD--\n" + board)
        board
      }
      boards
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }

}
