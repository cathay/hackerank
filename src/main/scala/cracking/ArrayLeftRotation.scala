package cracking

object ArrayLeftRotation {

  def main(args: Array[String]): Unit = {

    val input = Array(1,2,3,4,5)
    println (rotate(input, 4).mkString("[",",","]"))
    //println (rotate(input, 3).mkString("[",",","]"))
   // println (rotate(input, 5).mkString("[",",","]"))

  }

  def rotate(arrays: Array[Int], num_of_rotations: Int): Array[Int] = num_of_rotations match {
    case x if x <= 0 => throw new IllegalArgumentException("rotation should not be less or equal 0")
    case _ => num_of_rotations -  arrays.length match {
      case 0 => arrays.reverse
      case x if x > 0  => throw new IllegalArgumentException("Rotation is larger than size of array")
      case _ =>  {
          val head = arrays.drop(num_of_rotations)
          val tail = arrays.dropRight(arrays.length - num_of_rotations)

          //println(head.mkString("[",",","]"))
          //println (tail.mkString("[",",","]"))
          val result = head ++ tail
          println(result.mkString(","))
          result
        }
      }
  }
}
