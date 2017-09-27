package statistics

import scala.math.BigDecimal.RoundingMode

object StandardDeviation {

  import Statistics._

  def sd(arrays: Array[Int]) = {
    val m = mean(arrays)

    val sd = arrays.map(x => Math.pow(x - m, 2))
      .sum / arrays.length

    BigDecimal(Math.sqrt(sd)).setScale(1, RoundingMode.HALF_UP)
  }

}
