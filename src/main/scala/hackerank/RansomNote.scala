package hackerank

import scala.collection.mutable.ArrayBuffer

object RansomNote {

  def main(args: Array[String]): Unit = {

    checkRansomNote("give me one grand today night", "give one grand today")
    checkRansomNote("two times three is not four", "two times two is four thay")
  }

  def checkRansomNote(magazineWords: Array[String], ransomWords: Array[String]): Unit = {
    val magazines = new ArrayBuffer() ++ magazineWords

    val result = ransomWords.map(find(_, magazines)).reduce(_ && _)
    if(result) println("Yes")
    else println("No")
  }

  def find(a: String, magazines: ArrayBuffer[String]): Boolean = {
    val index = magazines.indexOf(a)
    if (index != -1) {
      magazines.remove(index)
      return true
    }
    false
  }

  def checkRansomNote(first: String, second: String): Unit =
    checkRansomNote(first.split(" "), second.split(" "))

}
