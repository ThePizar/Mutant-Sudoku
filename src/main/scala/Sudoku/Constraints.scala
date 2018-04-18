package Sudoku

import org.chocosolver.solver.Model
import org.chocosolver.solver.variables.IntVar

object Constraints {
  /**
    * Create constraints for the rows of the given Board on the given model
    *
    * @param model the Model to add the constraints to
    * @param board the Board to get the rows of
    */
  def addRowConstraints (model : Model, board : Array[Array[IntVar]]) : Unit = {
    board.foreach(row => {
      model.allDifferent(row, "DEFAULT").post()
    })
  }

  /**
    * Create constraints for the columns of the given Board on the given model
    *
    * @param model the Model to add the constraints to
    * @param board the Board to get the columns of
    */
  def addColConstraints (model : Model, board : Array[Array[IntVar]]) : Unit = {
    val colCount = board.map(_.length).max
    0.until(colCount).foreach(idx => {
      val col = board.flatMap(row => {
        if (idx >= row.length) None
        else Some(row(idx))
      })
      model.allDifferent(col, "DEFAULT").post()
    })
  }

  /**
    * Create constraint Squares of given size of the given board for the given model
    * This constraint creator truncates any constraints to the right or below the last full square
    *
    * @param model the Model to add the constraints to
    * @param board the Board to get the squares of
    * @param size the Size of the Squares
    */
  def addSquareConstraints (model : Model, board: Array[Array[IntVar]], size : Int) : Unit = {
    val colCount = board.map(_.length).max
    val rowCount = board.length
    val latSqCount : Int = colCount / size
    val vertSqCount : Int = rowCount / size

    val base : Array[(Int, Int)] = {
      0.until(size).flatMap(col => {
        0.until(size).map(row => {
          (row, col)
        })
      }).toArray
    }

    val sqs : Array[Array[(Int, Int)]] = {
      0.until(latSqCount).flatMap(col => {
        0.until(vertSqCount).map(row => {
          base.map(pair => {
            (pair._1 + row * size, pair._2 + col * size)
          })
        })
      }).toArray
    }

    sqs.foreach(sq => {
      addAllDiffGroup(model, board, sq)
    })
  }

  /**
    * Create a constraint for the given model from the given list and it's indexes
    *
    * @param model the Model to add the constraints to
    * @param list the List to make the constraint from
    * @param idxs the Indexes of the Variables in the list
    */
  def addAllDiffGroup (model : Model, list: Array[IntVar], idxs : Array[Int]) : Unit = {
    var group : Array[IntVar] = idxs.flatMap(idx => {
      if (idx > list.length) None
      else Some(list(idx))
    })
    if (group.nonEmpty) model.allDifferent(group, "DEFAULT").post()
  }

  /**
    * Create a constraint for the given model from the given board and it's locations
    *
    * @param model the Model to add the constraints to
    * @param board the Board to make the constraint from
    * @param locs the Locations of the Variables in the Board
    */
  def addAllDiffGroup (model : Model, board: Array[Array[IntVar]], locs : Array[(Int, Int)]) : Unit = {
    val list : Array[IntVar] = locs.flatMap(utils.atLoc(board, _))
    if (list.nonEmpty) model.allDifferent(list, "DEFAULT").post()
  }

  /**
    * Create a constraint that the smaller will be less than larger
    *
    * @param model the Model to add the constraints to
    * @param smaller the smaller IntVar
    * @param larger the larger IntVar
    */
  def addComparisonPair (model: Model, smaller : IntVar, larger: IntVar) : Unit = {
    model.arithm(smaller, "<", larger).post()
  }

  def ordered (model: Model, toOrdered: Array[IntVar]) : Unit = {
    model.lexChainLess(toOrdered.map(Array(_)).toSeq:_*).post() //Probably works?

    //Back up:
//    0.until(toOrdered.length - 1).foreach(i => {
//      addComparisonPair(model, toOrdered(i), toOrdered(i + 1))
//    })
  }

  /**
    * Create a constraint the variables are off by one
    *
    * @param model the Model to add the constraints to
    * @param first an IntVar
    * @param second another IntVar
    */
  def deltaOne (model: Model, first : IntVar, second: IntVar) : Unit = {
    delta(model, first, second, 1)
  }

  /**
    * Create a constraint the variables are off a constant
    *
    * @param model the Model to add the constraints to
    * @param first an IntVar
    * @param second another IntVar
    * @param delta the difference between the numbers
    */
  def delta (model: Model, first : IntVar, second : IntVar, delta: Int) : Unit = {
    //TODO replace with own propagator?
    model.or(
      model.allEqual(first, model.intOffsetView(second, delta)),
      model.allEqual(first, model.intOffsetView(second, delta * -1))
    ).post()
  }

  /**
    * Create a constraint that allows only one of the variables to take on the value
    *
    * @param model the Model to add the constraints to
    * @param vars the IntVars that are being constrained
    * @param value the value a variable must hold
    */
  def oneOfIs (model: Model, vars : Array[IntVar], value : Int) : Unit = {
    model.count(value, vars, model.intVar(1)).post()
  }

  /**
    * Create a constraint that force the values to sum up to the given value
    *
    * @param model the Model to add the constraints to
    * @param vars the IntVars that are being constrained
    * @param value the value the variables must sum up to
    */
  def sumTo (model : Model, vars : Array[IntVar], value: Int) : Unit = {
    model.sum(vars, "=", value).post()
  }

  /**
    * Create a constraint that force the values to sum up to the given value
    *
    * @param model the Model to add the constraints to
    * @param vars the IntVars that are being constrained
    * @param value the value the variables must sum up to
    */
  def sumTo (model : Model, vars : Array[IntVar], value: IntVar) : Unit = {
    model.sum(vars, "=", value).post()
  }
}
