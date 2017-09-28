package algorithms.warmup


object BirthdayCakeCandle {

  def main(args: Array[String]): Unit = {
    println(findCandles(Array(3,2,1,3)))
  }

  def findCandles(candles: Array[Int]) = {
    val maxHeight = candles.maxBy(p => p)

    candles.filter(_ == maxHeight).length
  }
}
