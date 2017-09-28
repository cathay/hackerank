package algorithms.warmup

object HurdleRace {

  def main(args: Array[String]): Unit = {

    println(detectMinimBeverage(Array(1, 6, 3, 5, 2), 4))
    println(detectMinimBeverage(Array(2, 5, 4, 5, 2), 7))
  }

  def detectMinimBeverage(hurdles: Array[Int], racerCapacity: Int) = {
    val maxHeight = hurdles.maxBy(p => p)

    maxHeight - racerCapacity match {
      case x if x<= 0 => 0
      case x => x
    }
  }
}
