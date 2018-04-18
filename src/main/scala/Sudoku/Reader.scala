package Sudoku

import org.chocosolver.solver.Model
import org.chocosolver.solver.variables.IntVar

object Reader {
  def createClassic (input: Array[String]): (Model, Array[Array[IntVar]]) = {
    if (input.head.toLowerCase != "sudoku") throw new IllegalArgumentException("Not a Sudoku puzzle")
    val model = new Model("Sudoku Classic")

    val order = input(1).toInt
    val size = order * order

    //Read in board
    val numBoard : Array[Array[Int]] = {
      input.slice(2, 2 + size).map(row => { //Ignore first two rows
        val ret = row.split(" ").map(num => num.toInt)
        if (ret.length != size) throw new IllegalArgumentException("Not a complete Sudoku puzzle")
        ret
      })
    }
    if (numBoard.length != size) throw new IllegalArgumentException("Not a complete Sudoku puzzle")

    //Get the locations of known numbers
    val filled : Array[(Int, Int, Int)] =
      numBoard.indices.flatMap(row => {
        numBoard(row).indices.flatMap(col => {
          numBoard(row)(col) match {
            case 0 => None
            case n => Some((row, col, n))
          }
        })
      }).toArray

    val board : Array[Array[IntVar]] = model.intVarMatrix("tiles", size, size, Array.range(1, size + 1))

    //Fill in known values
    filled.foreach(entry => {
      val (row, col, num) = entry
      board(row)(col) = model.intVar(num)
    })

    //println(board(0)(8).getDomainSize)
    //Constraints.addAllDiffGroup(model, board, 0.to(8).map((0, _)).toArray)

    Constraints.addRowConstraints(model, board)
    Constraints.addColConstraints(model, board)
    Constraints.addSquareConstraints(model, board, order)
    (model, board)
  }

  def createSujiken (input: Array[String]): (Model, Array[Array[IntVar]]) = {
    if (input.head.toLowerCase != "sujiken") throw new IllegalArgumentException("Not a Sujiken puzzle")
    val model = new Model("Sujiken")

    val order = input(1).toInt
    val size = order * order

    //Read in board
    val numBoard : Array[Array[Int]] = {
      var count = 1
      input.slice(2, 2 + size).map(row => { //Ignore first two rows
        val ret = row.split(" ").map(num => num.toInt)
        if (ret.length != count) throw new IllegalArgumentException("Not a valid Sujiken puzzle")
        count += 1
        ret
      })
    }
    if (numBoard.length != size) throw new IllegalArgumentException("Not a valid Sujiken puzzle")

    //Get the locations of known numbers
    val filled : Array[(Int, Int, Int)] =
      numBoard.indices.flatMap(row => {
        numBoard(row).indices.flatMap(col => {
          numBoard(row)(col) match {
            case 0 => None
            case n => Some((row, col, n))
          }
        })
      }).toArray

    val board : Array[Array[IntVar]] = {
      1.to(size).map(row => {
        model.intVarArray(f"row $row", row, Array.range(1, size + 1))
      }).toArray
    }

    //Fill in known values
    filled.foreach(entry => {
      val (row, col, num) = entry
      board(row)(col) = model.intVar(num)
    })

    val diags = 0.until(size).flatMap(top => {
      List(
        //Down-right diags
        0.to(top).map(idx => {
          (size - idx - 1, top - idx)
        }).toArray,
        //Up-right diags longs
        0.until(size).map(row => {
          (top + row, top - row)
        }).filter({case (x, y) => x < size && y >= 0}).toArray,
        0.until(size).map(row => {
          (top + row + 1, top - row)
        }).filter({case (x, y) => x < size && y >= 0}).toArray
      )
    }).toArray

//    diags.foreach(diag => {
//      println(diag.mkString(" "))
//    })

    //Sudoku.printBoard(board)

    diags.foreach(diag => {
      Constraints.addAllDiffGroup(model, board, diag)
    })

    Constraints.addRowConstraints(model, board)
    Constraints.addColConstraints(model, board)
    Constraints.addSquareConstraints(model, board, order)
    (model, board)
  }

  def createArbitrary (input: Array[String]): (Model, Array[Array[IntVar]]) = {
    if (input.head.toLowerCase != "custom") throw new IllegalArgumentException("Not a custom Sudoku puzzle")
    val model = new Model("Custom Sudoku")

    val max = input(1).toInt
    val rowCount = input(2).toInt

    //Read in board
    val numBoard : Array[Array[Int]] = {
      input.slice(3, 3 + rowCount).map(row => { //Ignore first two rows
        val ret = row.split(" ").map(num => num.toInt)
        ret
      })
    }

    val colCount = numBoard.map(_.length).max

    //Get the locations of known numbers
    val filled : Array[(Int, Int, Int)] =
      numBoard.indices.flatMap(row => {
        numBoard(row).indices.flatMap(col => {
          numBoard(row)(col) match {
            case 0 => None
            case n => Some((row, col, n))
          }
        })
      }).toArray

    val board : Array[Array[IntVar]] = model.intVarMatrix("tiles", rowCount, colCount, Array.range(1, max + 1))

    //Fill in known values
    filled.foreach(entry => {
      val (row, col, num) = entry
      board(row)(col) = model.intVar(num)
    })

    val groups = input.drop(3 + rowCount)
    groups.foreach(group => {
      val items = group.split(" ")
      items(0) match {
        case "col" => Constraints.addColConstraints(model, board)
        case "row" => Constraints.addRowConstraints(model, board)
        case "squ" => {
          if (items.length == 1 || !items(1).forall(_.isDigit)) throw new IllegalArgumentException("`Square` must have a size")
          else Constraints.addSquareConstraints(model, board, items(1).toInt)
        }
        case "grp" => {
          val pairs = parsePairs(items.drop(1))
          Constraints.addAllDiffGroup(model, board, pairs)
        }
        case "dl1" => {
          val vars = parsePairs(items.drop(1)).map(utils.mustAtLoc(board, _))
          if (vars.length != 2) throw new IllegalArgumentException("Vars for delta is not a pair")
          Constraints.deltaOne(model, vars(0), vars(1))
        }
        case "1of" => {
          if (items.length == 1 || !items(1).forall(_.isDigit)) throw new IllegalArgumentException("`One of` must have a number")
          val vars = parsePairs(items.drop(2)).map(utils.mustAtLoc(board, _))
          Constraints.oneOfIs(model, vars, items(1).toInt)
        }
        case "leq" => {
          val vars = parsePairs(items.drop(1)).map(utils.mustAtLoc(board, _))
          Constraints.addComparisonPair(model, vars(0), vars(1))
        }
        case "snk" => {
          val vars = parsePairs(items.drop(1)).map(utils.mustAtLoc(board, _))
          Constraints.ordered(model, vars)
        }
        case "sum" => {
          if (items.length == 1 || !items(1).forall(_.isDigit)) throw new IllegalArgumentException("`Sum` must have a total")
          val vars = parsePairs(items.drop(2)).map(utils.mustAtLoc(board, _))
          Constraints.sumTo(model, vars, items(1).toInt)
        }
        case _ => throw new IllegalArgumentException(s"Not a recognized group type ${items(0)}")
      }
    })

    //model.getCstrs.foreach(cons => {println(cons.toString)})

    (model, board)
  }

  def parsePairs(strings: Array[String]) : Array[(Int, Int)] = {
    strings.map(strPair => {
      val pair = strPair.split(',').map(_.toInt)
      (pair(0), pair(1))
    })
  }
}
