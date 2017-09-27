package cracking

object SherlockArray {

  def main(args: Array[String]): Unit = {

    sherlock(Array(3, 4, 5, 1, 3, 4, 4, 1))
    sherlock(Array(3, 2, 1, 2, 1, 2))
    sherlock(Array(3, 5, 1, 2, 7, 1, 1))
    sherlock(Array(1))
    sherlock(Array(1,2,3,2,1))
    sherlock(Array(1, 2, 3, 3))
    sherlock(Array(1, 2, 3, 3, 6))
    sherlock(Array(1, 2, 1))
    sherlock(Array(3, 4, 4, 1, 10, 3, 4, 5))
    sherlock(Array(0,0,0,0,1,0))
    sherlock(Array(-1,1,1,0,1))
  }

  def sherlock(arrays: Array[Int]): Unit = {
    if(arrays.length == 2) {
      println("NO")
      return
    }

    if(arrays.length == 1) {
      println("YES")
      return
    }

    sherlock(arrays, arrays.length/2) match {
      case true => println("YES")
      case false => println("NO")
    }
  }

  def sherlock(arrays: Array[Int], index: Int):Boolean = {

    val right = arrays.drop(index + 1)
    val left = arrays.length % 2 match {
      case x if x == 0 =>  arrays.dropRight(index)
      case _ =>  arrays.dropRight(index + 1)
    }

    var sumLeft = left.sum
    var sumRight = right.sum

    //println(sumLeft + ":" + sumRight)

    if(sumLeft < sumRight) {
      var i = index
      while(i < arrays.length - 1) {
        sumLeft = sumLeft + arrays(i)
        sumRight =  sumRight - arrays(i+1)
        //println(sumLeft + ":" + sumRight)
        if (sumLeft == sumRight) return true
        i = i + 1
      }
      return false
    }

    if(sumLeft > sumRight) {
      var i = index - 1
      while(i > 0) {
        sumLeft = sumLeft - arrays(i)
        sumRight =  sumRight + arrays(i + 1)
       // println(sumLeft + ":" + sumRight)
        if (sumLeft == sumRight) return true
        i = i - 1
      }
      return false
    }

    return true

  }
}

