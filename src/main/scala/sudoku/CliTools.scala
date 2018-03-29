package sudoku

import sudoku.Solver._

import scala.io.StdIn

object Main extends App {

  // cli arguments
  case class Config(boardSize: Int = -1, performance: Boolean = false)

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("Sudoku Solver")

    opt[Int]('s', "size").action( (x, c) =>
      c.copy(boardSize = x) ).text("number of characters per row/column/quadrant")

    opt[Boolean]('s', "performance").action( (x, c) =>
      c.copy(performance = x) ).text("show the average solve time per sudoku")

    help("help").text("prints this usage text")

  }

//  def recordTime()

  // parser.parse returns Option[C]
  parser.parse(args, Config()) match {
    case Some(config) => {
      val results = Iterator.continually(StdIn.readLine())
        .takeWhile(line => line != null && line.nonEmpty)
        .map(line => {
          val boardTokens = line.toList
          val startTime = System.nanoTime
          val solution = loadSudoku(boardTokens).flatMap(b => solve(b))
          val endTime = System.nanoTime
          val time = (endTime - startTime) / 1000
          if (solution.nonEmpty) println(linePrint(solution.get)) else println()
          (solution, time)
        }).toList

      val count = results.length
      val total = results.map(x => x._2).sum.toDouble

      println(s"Finished, $count sudokuseys solved\nAverage time: ${total / count} microseconds")

    }
    // do stuff

    case None =>
    // arguments are bad, error message will have been displayed
  }


}
