package statistics

import java.math.MathContext
import java.util.Scanner

import scala.math.BigDecimal.RoundingMode

object Statistics {

  def mean(arrs: Array[Int]): Double = arrs.length match {
    case 0 => 0
    case x => Math.round(arrs.sum.toDouble/x * 100.0)/100.0
  }

  def median(arrs: Array[Int]): Double = arrs.length match {
    case 0 => 0
    case x => {
      val sortedArrays = arrs.sortWith(_ < _ )
      x % 2 match {
        case 0 => Math.round((sortedArrays(x/2 -1) + sortedArrays(x/2)).toDouble/2 * 100.0)/100.0
        case 1 => sortedArrays(x/2).toDouble
      }
    }
  }

  def mode(arrs: Array[Int]): Int = arrs.length match {
    case 0 => 0
    case _ => {
      val modes = arrs.map(i => (i, 1))
        .groupBy(_._1)
        .mapValues(_.length)

      val firstMode = modes.maxBy(_._2)
      val result = modes.filter(_._2 ==  firstMode._2)
        .toList
        .sortWith(_._1 < _._1)

      //result.foreach(println)
      result(0)._1
    }
  }

  def weightMean(arrays: Array[Int], weights: Array[Int]) = arrays.length == weights.length match {
    case false => throw new IllegalArgumentException("Two vectors need to have the same size")
    case true => weights.sum match {
      case 0 => 0.0
      case _ => {

        val sumOfProduct = (arrays zip weights)
          .map(p => p._1 * p._2)
          .sum

        BigDecimal(sumOfProduct.toDouble/weights.sum).setScale(2, RoundingMode.HALF_EVEN)
      }
    }
  }


//  def main(args: Array[String]): Unit = {
//    val sc = new Scanner(System.in)
//    val stdin = scala.io.StdIn
//    println(stdin.readLine() + 1)
//
//    println(BigDecimal(sc.nextDouble).setScale(1, RoundingMode.HALF_EVEN))
//    val n = sc.nextInt
//    var a = new Array[Int](n);
//    for(a_i <- 0 to n-1) {
//      a(a_i) = sc.nextInt();
//    }
//
//    val arrays = Array(64630, 11735, 14216, 99233,
//      14470, 4978, 73429, 38120, 51135, 67060)
//
//    println(mean(arrays))
//    println(median(arrays))
//    println(mode(arrays))
//
//    println(weightMean(Array(10, 40, 30, 50, 20), Array(1, 2, 3, 4, 5)))
//    println(weightMean(Array(1, 2, 3), Array(0, 1, 2)))
//
//    println(BigDecimal(.1) + BigDecimal(.2))
//  }

  def main(args: Array[String]): Unit = {
    val stdin = scala.io.StdIn
    val mealCost = stdin.readDouble()
    val tipPercent = stdin.readInt()
    val taxPercent = stdin.readInt()

//    println(BigDecimal(mealCost * tipPercent/100).setScale(1, RoundingMode.HALF_UP).doubleValue)
//    println(BigDecimal(mealCost * taxPercent/100).setScale(1, RoundingMode.HALF_UP).doubleValue)
    val totalCost = mealCost + mealCost * tipPercent/100 + mealCost * taxPercent/100
    //+ BigDecimal(mealCost * tipPercent/100).setScale(1, RoundingMode.HALF_UP).doubleValue
    //+ BigDecimal(mealCost * taxPercent/100).setScale(1, RoundingMode.HALF_UP).doubleValue

    println(Math.round(BigDecimal(totalCost).setScale(2, RoundingMode.HALF_UP).doubleValue()))
  }
}
