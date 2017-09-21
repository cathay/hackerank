package test

import org.scalatest.FunSuite
import TriangleClassifier._
import Triangle._

class TriangleClassifierTest extends FunSuite {

  test("when the input is negative number") {
    assertThrows[IllegalArgumentException](check((-1, -1 , 2)))
  }

  test("when the input is 0") {
    assertThrows[IllegalArgumentException](check((0, -1 , 2)))
  }

  test("when the the triangle is error") {
    check(2, 3 , 6) == error
  }

  test("when the the triangle is equilateral") {
    check(2, 2 , 2) == error
  }

  test("when the the triangle is isosceles") {
    check(2, 3 , 2) == error
  }

  test("when the the triangle is scalene") {
    check(2, 3 , 4) == scalene
  }
}
