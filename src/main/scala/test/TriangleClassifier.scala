package test

object TriangleClassifier {
  import Triangle._

  def check(sides: (Int, Int, Int)): Triangle = {

    if(sides._1 <=0 || sides._2 <= 0 || sides._3 <= 0)
      throw new IllegalArgumentException("Sides of a triangle must be larger than 0")

    if(
      (sides._1 + sides._2 <= sides._3) ||
      (sides._1 + sides._3 <= sides._2) ||
      (sides._3 + sides._2 <= sides._1))
      error

    if(sides._1 == sides._2 && sides._2 == sides._3)
      equilateral

    if(
      (sides._1 == sides._2) ||
      (sides._2 == sides._3) ||
      (sides._1 == sides._3))
      isosceles

    scalene
  }
}

object Triangle extends Enumeration {

  type Triangle = Value

  val scalene = Value(1)
  val isosceles = Value(2)
  val equilateral = Value(3)
  val error =  Value(4)
}
