package AI.bot

object BotPrincess {

  import Move._
  def main(args: Array[String]): Unit = {
    pathToPrincess(
      Array(
        "---",
        "-m-",
        "p--"
      ),
      Tuple2(1,1)
    )

    println("............")
    pathToPrincess(Array(
        "-----",
        "-----",
        "p--m-",
        "-----",
        "-----"
    ), (2, 3))
  }

  def pathToPrincess(grid: Array[String], heroPos:(Int, Int)) = {
    val princessPos = getPrincessPos(grid)
    //val heroPos = (grid.length/2, grid.length/2)

    val movesX = princessPos._2 > heroPos._2 match {
      case true => (1 to (princessPos._2 - heroPos._2)).map(x => RIGHT)
      case false => (1 to (heroPos._2 - princessPos._2 )).map(x => LEFT)
    }

    val movesY = princessPos._1 > heroPos._1 match {
      case true => (1 to (princessPos._1 - heroPos._1)).map(x => DOWN)
      case false => (1 to (heroPos._1 - princessPos._1)).map(x => UP)
    }

    movesX.size >= movesY.size match {
      case true => println(movesX(0))
      case false => println(movesY(0))
    }
    //movesX.foreach(println)
   // movesY.foreach(println)
  }

  def getPrincessPos(grid: Array[String]): (Int, Int) = {
    val pos = for {
      i <- 0 until grid.length
      j <- 0 until grid(i).length

      if (grid(i).charAt(j).equals('p'))
    } yield (i, j)

    pos.toList.head
  }
}

object Move extends Enumeration{
  type Move = Value
  val LEFT, RIGHT, UP, DOWN = Value
}


