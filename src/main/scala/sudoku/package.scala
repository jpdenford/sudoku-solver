import scala.util.matching.Regex

package object sudoku {
  val dictionaryPath = List("sudoku", "sudokus.txt")

  def loadSudoku: List[Solver.Board] = {
    val wordstream = Option {
      getClass.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val commentReg = "#.*"
      val s = io.Source.fromInputStream(wordstream)
      val allLines = s.getLines.filter(l => !(l.isEmpty || l.matches(commentReg))).toVector
      val linesByBoard = allLines.grouped(9).toList
      val boards = linesByBoard.map { b =>
        val boardLines = b.map(
          line => {
            val ns = line.toVector
            //split string into chars
            val sets = ns.map({
              case '0' => (1 until 10).toSet
              case n => Set(Integer.parseInt(n.toString))
            })
            sets
          })
        val board = Solver.Board(boardLines)
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
