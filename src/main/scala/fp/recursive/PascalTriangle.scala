package fp.recursive

object PascalTriangle {

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 6) {
      println(generate(i).mkString(" "))
    }

  }

  def generate(n: Int): List[Int] = n <=3 match {

    case true if n == 1 => List(1)
    case true if n == 2 => List(1,1)
    case true if n == 3 => List(1,2,1)
    case false => nextRow(generate(n-1))
  }

  def nextRow(pascalRow: List[Int]): List[Int] = {
   1 +: (pascalRow.drop(1) zip pascalRow.dropRight(1)).map(p => p._2 + p._1) :+ 1
  }
}
