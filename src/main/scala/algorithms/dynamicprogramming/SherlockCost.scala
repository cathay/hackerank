package algorithms.dynamicprogramming

import scala.collection.mutable.ArrayBuffer

object SherlockCost {

  def main(args: Array[String]): Unit = {
    //sherlock(Array(10, 1, 10, 1, 10)).foreach(println)

    println(sherlock(Array(2,4,1,2,7,7)).mkString(","))
    println(distance(Array(10, 1, 10, 1, 10)))
  }

  def distance(arrays: Array[Int]) = {
    (arrays.drop(1) zip arrays.dropRight(1))
      .map(p => Math.abs(p._1 -  p._2))
      .sum
  }

  def sherlock(b: Array[Int]) = {
    println(b.mkString(","))
    val buffer = new ArrayBuffer[Int] ++ b
    for(i <- 0 until buffer.length) {
      if(i % 2 == 1) buffer(i) = 1
    }
    buffer.toArray
  }
}
