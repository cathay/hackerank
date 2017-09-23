package hackerank

object Annagrams {

  def main(args: Array[String]): Unit = {
//    countMinDeletionToMakeAnagramPair(" ccde", "abc12")
//    countMinDeletionToMakeAnagramPair("a", "aaaaaa")
//    countMinDeletionToMakeAnagramPair("ab", "aabaaaa")
//    countMinDeletionToMakeAnagramPair("abc", "aabaaaa")
//    countMinDeletionToMakeAnagramPair("abc", "def")
    countMinDeletionToMakeAnagramPair(
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabjkdafklkgflksngdjfidifhoidkfnakfdafadkfdakfkldallknlafsakfjsajkjfklsafalfjalfjkajskfjalfasjkjfklas" +
      "jfnajkfjkasjfsajkfajkjfnajkfjkasjfsajkfajkjfnajkfjkasjfsajkfajkjkdjkhajkdhsajfkjahjfakkioqedncxkkiiqwewouoiasdaasaaafdfdafdfafa" +
        "fdjakfljalkjfiqiqoncklaskanvlalaspoaopaijsfidajfoidahfnandafdafjlkdajfkdajfioiohiadhifiqeohreiqeierohqoirheqirheqiroheqrheqrq",

       "jhdjsahjdaacbck")
  }

  def countMinDeletionToMakeAnagramPair(first: String, second: String) = {

    if(first ==null  || second == null || first.isEmpty || second.isEmpty)
      throw new IllegalArgumentException("No way to find")

    val groupFirstByLetter = first.toStream
      .map(c => (c,1))
      .groupBy(p => p._1)
      .mapValues(_.size)

    val groupSecondByLetter = second.toStream
      .map(c => (c,1))
      .groupBy(p => p._1)
      .mapValues(_.size)

    println(groupFirstByLetter)
    println(groupSecondByLetter)

    val notExistingAtSecond = diff(groupFirstByLetter, groupSecondByLetter)
    val notExistingAtFirst = diff(groupSecondByLetter, groupFirstByLetter)

    val firstCommonPart = groupFirstByLetter
      .filterKeys(c => notExistingAtSecond.getOrElse(c, -1) == -1)

    val secondCommonPart = groupSecondByLetter
      .filterKeys(c => notExistingAtFirst.getOrElse(c, -1) == -1)

    if(firstCommonPart.isEmpty || secondCommonPart.isEmpty)
      throw new IllegalArgumentException("No way to find")

    val count = firstCommonPart
      .map(p => Math.abs(secondCommonPart.get(p._1).get - p._2))
      .reduce(_ + _) + aggregate(notExistingAtSecond) + aggregate(notExistingAtFirst)

    println(count)
  }

  def aggregate(letters: Map[Char, Int]) = {
    letters.aggregate(0)(
      (b, p) => b + p._2,
      (b1, b2) => b1 + b2
    )
  }

  def diff(letters1: Map[Char,Int], letters2: Map[Char, Int]) = {
    letters1.filter({
      case (c: Char, _) => {
        val count = letters2.getOrElse(c, -1)
        count == -1
      }
    })
  }

}
