package cracking

import scala.util.control._
import util.control.Breaks._

object BubbleSort {

  def main(args: Array[String]): Unit = {
   // sort(Array(3, 2, 1))
   // sort(Array(1, 2, 3))
    sort(Array(4, 2, 3, 1, 5))
  }

  def sort(arrays: Array[Int]) = {
    var swaps = 0
    for (i <- 0 until arrays.length) {
      var numberOfSwaps = 0
      val loop = new Breaks
      breakable {
        for(j <- 0 until arrays.length -1) {
          if(arrays(j) > arrays(j+1)) {
            swap(arrays, j, j+1)
            numberOfSwaps += 1
            swaps += 1
          }
          println(arrays.mkString(","))
          //swaps += numberOfSwaps
          if(numberOfSwaps == 0) {
            println("break")
            break
          }
        }
      }
    }

    println(s"Array is sorted in $swaps swaps.")
    println(s"First Element: ${arrays(0)}")
    println(s"Last Element: ${arrays(arrays.length - 1)}")
  }

  def swap(arrays: Array[Int], i: Int, j:Int) = {
    val temp = arrays(i)
    arrays(i) = arrays(j)
    arrays(j) = temp
  }
}
