package statistics

import scala.math.BigDecimal.RoundingMode

object Quartiles {

  def interQuartiles(arrays: Array[Int]) = {

    val sortedArrays = arrays.sortWith(_ <= _)

    sortedArrays.length % 2 match {
      //even set of numbers
      case 0 => {
        val medianIndex = sortedArrays.length/2

        val Q1 = medianSortedArray(sortedArrays, 0, medianIndex -  1)
        val Q2 = medianSortedArray(sortedArrays, 0, sortedArrays.length - 1)
        val Q3 = medianSortedArray(sortedArrays, medianIndex, sortedArrays.length - 1)

        (Q1, Q2, Q3)

      }

      //odd set of numbers
      case _ => {
        val medianIndex = sortedArrays.length/2
        val Q1 = medianSortedArray(sortedArrays, 0, medianIndex - 1)
        val Q2 = medianSortedArray(sortedArrays, 0, sortedArrays.length - 1)
        val Q3 = medianSortedArray(sortedArrays, medianIndex + 1, sortedArrays.length - 1)

        (Q1, Q2, Q3)
      }
    }
  }

  def medianSortedArray(sortedArrays: Array[Int], beginIndex: Int, endIndex: Int): Double = {

    val lengthOfArrays = (endIndex - beginIndex + 1)
    val medianIndex = beginIndex + lengthOfArrays/2

    lengthOfArrays match {
      case x if x %2 == 0 => ((sortedArrays(medianIndex - 1) + sortedArrays(medianIndex)).toDouble/2)
      case y if y %2 == 1 => sortedArrays(medianIndex)
    }
  }

  def interQuartile(arrays: Array[Int], frequencies: Array[Int]) = {
    if(arrays.length != frequencies.length) throw new IllegalArgumentException("input and frequencies need to have same size")

    val elements = (arrays zip frequencies) map (p =>  Array.fill(p._2)(p._1)) flatMap(x=> x)
    val (q1, _, q3) = interQuartiles(elements)

    BigDecimal(q3).setScale(1, RoundingMode.HALF_UP) - BigDecimal(q1).setScale(1, RoundingMode.HALF_UP)
  }

  def medianSortedArray(sortedArrays: Array[Int]): Double = sortedArrays.length match {
    case 0 => 0.0
    case x => {
      x % 2 match {
        case 0 => (sortedArrays(x/2 -1) + sortedArrays(x/2))/2
        case 1 => sortedArrays(x/2)
      }
    }
  }

  def median(arrays: Array[Int]): Double = arrays.length match {
    case 0 => 0.0
    case x => {
      val sortedArrays = arrays.map(_.toDouble).sortWith(_ < _ )
      x % 2 match {
        case 0 => (sortedArrays(x/2 -1) + sortedArrays(x/2))/2
        case 1 => sortedArrays(x/2)
      }
    }
  }

  def main(args: Array[String]): Unit = {
//    println(quartiles(Array(5, 7, 4, 4, 6, 2, 8)))
//    println(quartiles(Array(1, 3, 3, 4, 5, 6, 6, 7, 8, 8)))
//    println(quartiles(Array(6, 7, 15, 36, 39, 40, 41, 42, 43, 47, 49)))
//    println(quartiles(Array(7, 15, 36, 39, 40, 41)))
//
     println(interQuartile(Array(6,12, 8, 10, 20, 16), Array(5, 4, 3, 2, 1, 5)))
//    println(interQuartile(Array(2,3), Array(1,0)))

    //println(quartiles(Array(2,3)))
    println(interQuartiles(Array(2,2,2,3,3,4)))
    println(interQuartiles(Array(2,2,2,3)))
    println(interQuartiles(Array(2)))
    //println(quartiles(Array(3, 5, 7, 8, 9, 11, 15, 16, 20, 21)))
  }
}
