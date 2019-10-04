package Concordance

/* THINGS I APPRECIATE ABOUT FUNCTIONAL PROGRAMMING:
*
*  The ability to apply many functions to a method quickly, e.g. when initiating variables etc
*
*  The code is very readable and I understand exactly what the methods are doing and being applied to.
*
*  Immutable types are safer than mutable types meaning less will go wrong,
*  I didn't (to my knowledge) use any mutable types in this program meaning
*  there has been less room for error.
*
* */

object Concordance {
  import scala.io.Source
  import scala.io.StdIn
  import lib._

  def main(args: Array[String]): Unit = {

    // Keywords used for Concordance. Split and set to List
    val keywordList = ("abstract case catch class def do else extends false final finally for " +
      "forSome if implicit import lazy match new null object override package private protected " +
      "return sealed super this throw trait try true type val var while with yield").split(" ").toList

    println("Enter the directory of the file you wish to run: \n")
    val path = StdIn.readLine() // allow user input

    // init & filter KW to map
    val filterKW = Source.fromFile(path)
      .getLines
      .zipWithIndex
      .foldLeft(Map.empty[String, Seq[Int]].withDefaultValue(Seq.empty[Int])) {
        case (map, (line, index)) =>
          val inner = line.split("\\W+").toSeq
            .filter(keywordList.contains)
            .groupBy(identity)
            .mapValues(_.map(_ => index + 1))
            .withDefaultValue(Seq.empty[Int])
          keywordList.map(kw => (kw, map(kw) ++ inner(kw))).toMap
      }

    println(Picture("Keywords").fixWidth(21) + Picture("Count").fixWidth(20) + Picture("Lines")) // fixed width 21 allows column titles to be lined up correctly
    keywordList.sorted.foreach { kw => if (filterKW(kw).nonEmpty) {  // nonEmpty aka != 0, stops output of keywords that aren't present in the read file.
        val picKW = Picture(kw).fixWidth(20)
        val picCount = Picture(filterKW(kw).length).fixWidth(20)
        val picLines = Picture(filterKW(kw).distinct.mkString("[", ", ", "]")).fixWidth(100)
        val picAll = picKW + picCount + picLines

        println(picAll.frameL) // Formatted Concordance

      }
    }
  }
}
