package Sudoku
/*
import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._

@RunWith(classOf[JUnitRunner])
class SudokuSuite extends FunSuite with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    import Sudoku._
  }

  test("Simple test") {
    import Sudoku._

    val rdd = List(1, 2, 4, 6).toArray
    val res = addOneCollect(rdd)
    assert(res === Array(2, 3, 5, 7))
  }
}
*/