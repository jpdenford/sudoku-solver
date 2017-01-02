import sudoku.Solver._

import scala.util.matching.Regex

package object sudoku {
  val dictionaryPath = List("sudoku", "sudokus.txt")

  private val emptyBoard = Vector.fill(9*9)((1 to 9).toSet)

  def loadSudoku: List[Board] = {
    val wordstream = Option {
      getClass.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      // set the squares on the board to their specified values
      def setupBoard(values: List[(Int, Int)], board: Board): Board = values match {
        case Nil => board
        case (n, i)::rest => setupBoard(rest, set(i, n, board))
      }

      val commentReg = "#.*"
      val s = io.Source.fromInputStream(wordstream)
      val allLines = s.getLines.filter(l => !(l.isEmpty || l.matches(commentReg))).toVector
      val linesByBoard = allLines.grouped(9).toList

      val boards = linesByBoard.map { b =>
        val boardLines = b.map (
          line => {
            val ns = line.toList
            //split string into chars
            val opts = ns.map({
              case '0' => None
              case n => Some(Integer.parseInt(n.toString))
            })
            opts
          })
        val zippedBoard = boardLines.flatten.zipWithIndex
        val values = for {
          elem <- zippedBoard
          value <- elem._1
        } yield (value, elem._2)
        val board = setupBoard(values.toList, emptyBoard)
        println("--LOADED BOARD--\n" + stringify(board))
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
