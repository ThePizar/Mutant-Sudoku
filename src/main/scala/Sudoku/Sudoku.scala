package Sudoku

import org.chocosolver.solver.{Model, Solution, Solver}
import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.search.strategy.selectors.values.{IntDomainBest, IntDomainImpact, IntDomainMin}
import org.chocosolver.solver.search.strategy.selectors.variables.{DomOverWDeg, FirstFail}

import scala.io.Source._

object Sudoku extends App {
  override def main(args: Array[String]): Unit = {

    val files = Array("outside_in_order.txt",
      "outside_inside_out.txt",
      "outside_pi_e.txt",
      "outside_powers_2.txt",
      "outside_triangles_squares.txt")

    files.foreach(fileName => {
      println(s"Processing $fileName...")
      val source = fromFile(s"./src/main/resources/$fileName")
      val strSource = source.getLines.toArray.filter(!_.startsWith("#"))
      source.close()

      val (sudokuModel : Model, board: Array[Array[IntVar]]) = strSource(0).toLowerCase match {
        case "sudoku" => Reader.createClassic(strSource)
        case "sujiken" => Reader.createSujiken(strSource)
        case "custom" => Reader.createArbitrary(strSource)
        case _ => throw new Exception("Unknown board type")
      }

      utils.printBoard(board)

      val solver : Solver = sudokuModel.getSolver

      var count = 0
      if(solver.solve()){
        //WDEG, IntDomainMin for search
        count += 1
        println("------------------------")
        utils.printBoard(board)
        println("----------WDEG----------")
        solver.printStatistics()
      }
      if (count == 0) {
        println("No Solutions found")
      }
      else {
        solver.reset()
        solver.setSearch(Search.intVarSearch(new FirstFail(sudokuModel), new IntDomainBest(), board.flatten.toSeq:_*))
        if(solver.solve()){
          println("----------MRV----------")
          solver.printStatistics()
        }
        solver.reset()
        solver.setSearch(Search.intVarSearch(new FirstFail(sudokuModel), new IntDomainImpact(), board.flatten.toSeq:_*))
        if(solver.solve()){
          println("----------LCV+MRV----------")
          solver.printStatistics()
        }
        solver.reset()
        solver.setSearch(new DomOverWDeg(board.flatten, 0, new IntDomainImpact()))
        if(solver.solve()){
          println("----------WDEG+LCV----------")
          solver.printStatistics()
        }
      }
    })
  }
}