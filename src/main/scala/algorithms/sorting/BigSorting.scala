package algorithms.sorting

object BigSorting {

  def main(args: Array[String]): Unit = {
    //println("31415926535897932384626433832795".toLong)
    sort(Array("31415926535897932384626433832795", "1", "3", "10", "5", "3"))

   println( Array(1, 4, 5, 7, 9, 12).indexOf(4))
  }

  def sort(numerics: Array[String]) = {
    numerics.sortWith(compare(_ , _))
      .foreach(println)

  }

  private def compare(a: String, b: String):Boolean = {

    if(a.length == b.length) {
      val numA = BigInt(a)
      val numB = BigInt(b)
      return numA < numB
    }
    a.length < b.length
  }


}
