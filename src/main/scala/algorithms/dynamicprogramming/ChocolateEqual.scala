package algorithms.dynamicprogramming

import scala.collection.mutable.ArrayBuffer

object ChocolateEqual {

  def main(args: Array[String]): Unit = {

    println(shares(Array(2, 2, 3, 7)))
    println(shares(Array(1,5,5)))

    val sc = new java.util.Scanner(System.in)
    val T = sc.nextInt
    var a0=0
    while(a0 < T) {
      val n = sc.nextInt
      var a = new Array[Int](n)
      for(a_i <- 0 to n-1) {
        a(a_i) = sc.nextInt()
      }
      a0 += 1
      println (a.mkString(" "))
     // println(shares(a))
    }
  }

  def shares(chocolates: Array[Int]) = {

    val min = chocolates.min
    val minChocolates = Array(min, min - 1, min - 2)

    val solutions = minChocolates.map(m => {
      chocolates.map(c => transform(c - m)).sum
    })

    solutions.min
  }

  /**
    * This is the way to calculate x + y + z where x, y, z satisfy
    * a = x + 2*y + 5*z
    */
  def transform(a: Int) = {
    val z = a / 5
    val y = (a % 5) / 2
    val x = (a % 5 % 2)/1

    x + y + z
  }
}
