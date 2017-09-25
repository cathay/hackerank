package cracking

import scala.collection.mutable.ListBuffer

object Annagrams {

  def main(args: Array[String]): Unit = {
//    countMinDeletionToMakeAnagramPair(" ccde", "abc12")
//    countMinDeletionToMakeAnagramPair("a", "aaaaaa")
    countMinDeletionToMakeAnagramPair("ab", "aabaaaa")
//    countMinDeletionToMakeAnagramPair("abc", "aabaaaa")
    countMinDeletionToMakeAnagramPair("abc", "def")
    countMinDeletionToMakeAnagramPair(
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabjkdafklkgflksngdjfidifhoidkfnakfdafadkfdakfkldallknlafsakfjsajkjfklsafalfjalfjkajskfjalfasjkjfklas" +
      "jfnajkfjkasjfsajkfajkjfnajkfjkasjfsajkfajkjfnajkfjkasjfsajkfajkjkdjkhajkdhsajfkjahjfakkioqedncxkkiiqwewouoiasdaasaaafdfdafdfafa" +
        "fdjakfljalkjfiqiqoncklaskanvlalaspoaopaijsfidajfoidahfnandafdafjlkdajfkdajfioiohiadhifiqeohreiqeierohqoirheqirheqiroheqrheqrq",

       "jhdjsahjdaacbck")

  }

  def countMinDeletionToMakeAnagramPair(first: String, second: String): Unit = {

    if(first ==null  || second == null || first.isEmpty || second.isEmpty) {
      println(0)
      return
    }

    val firstLetters = Array.fill(26){0}
    val secondLetters = Array.fill(26){0}
    first.toStream.foreach(c => firstLetters(c - 'a') = firstLetters(c - 'a') + 1)
    second.toStream.foreach(c => secondLetters(c - 'a') = secondLetters(c - 'a') + 1)

    val result = Array.fill(26){0}

    for(i <- 0 until firstLetters.length) {
      result(i) = firstLetters(i) - secondLetters(i)
    }

    println(result.reduce(Math.abs(_) + Math.abs(_)))
  }
}
