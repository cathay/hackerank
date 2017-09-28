package algorithms.warmup

object CatAndMouse {

  def main(args: Array[String]): Unit = {

    detectSpeed(1,2,3)
    detectSpeed(1,3,2)
    detectSpeed(2,1,3)
  }

  def detectSpeed(catA: Int, catB: Int, mouseC: Int): Unit = {
    val distance =  Math.abs(catA - mouseC) - Math.abs(catB - mouseC)

    distance match  {
      case 0 =>  println("Mouse C")
      case x if x > 0 => println("Cat B")
      case _ => println("Cat A")
    }
  }
}
