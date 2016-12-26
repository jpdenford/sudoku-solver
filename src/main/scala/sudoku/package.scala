package object sudoku {
  val dictionaryPath = List("sudoku", "sudokus.txt")

  def loadSudoku: Vector[Vector[Set[Int]]] = {
    val wordstream = Option {
      getClass.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      val lines = s.getLines.toVector.map(
        line => {
          val ns = line.toVector //split string into chars
          val sets = ns.map({
            case '0' => (1 until 10).toSet
            case n => Set(Integer.parseInt(n.toString))
          })
          sets
      })
      lines
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }

}
