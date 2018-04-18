package Sudoku

import org.chocosolver.solver.variables.IntVar

object utils {

  def atLoc (board: Array[Array[IntVar]], loc : (Int, Int)) : Option[IntVar] = {
    if (loc._1 >= board.length || loc._2 >= board(loc._1).length) None
    else Some(board(loc._1)(loc._2))
  }

  def mustAtLoc (board: Array[Array[IntVar]], loc : (Int, Int)) : IntVar = {
    if (loc._1 >= board.length || loc._2 >= board(loc._1).length)
      throw new IllegalArgumentException(f"${loc._1}, ${loc._2} does not exist in board")
    else board(loc._1)(loc._2)
  }

  def printBoard (board: Array[Array[Int]]) : Unit = {
    board.foreach(row => {
      println(row.mkString(" "))
    })
  }

  def printBoard (board: Array[Array[IntVar]]) : Unit = {
    board.foreach(row => {
      println(row.map(ivar => {
        if (ivar.isInstantiated) ivar.getValue
        else 0
      }).mkString(" "))
    })
  }
}
