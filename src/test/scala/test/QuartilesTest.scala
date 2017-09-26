package test

import org.scalatest.FunSuite
import statistics.Quartiles._

class QuartilesTest extends FunSuite {

  test("even set") {
    assert(interQuartiles(Array(2, 2, 2, 3, 4, 5)) === (2.0,2.5,4.0))
  }

  test("odd sets") {
    assert(interQuartiles(Array(2, 2, 2, 3, 4)) === (2.0,2.0,3.5))
    assert(interQuartiles(Array(1, 1, 2, 3, 4)) === (1.0,2.0,3.5))
  }

  test("2 element set") {
    assert(interQuartiles(Array(2, 2)) === (2.0,2.0,2.0))
    //assert(interQuartiles(Array(1, 1, 2, 3, 4)) === (1.0,2.0,3.5))
  }

}
