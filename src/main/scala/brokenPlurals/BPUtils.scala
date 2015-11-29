package brokenPlurals

import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
 * Created by becky on 11/28/15.
 */
object BPUtils {

  /**
   * Methods to Calculate Distances
   **/

  // From https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Scala
  // modified to use a weighted distance
  def weightedLevenshtein(str1: String, str2: String, table: HashMap[(String, String), Double]): Double = {
    val lenStr1 = str1.length
    val lenStr2 = str2.length

    val d: Array[Array[Double]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val a = decode(str1(i - 1).toString)
      val b = decode(str2(j - 1).toString)
      val cost: Double = if (str1(i - 1) == str2(j - 1)) 0 else table.getOrElse((a,b), table.getOrElse((b,a), -1.0))

      if (cost == -1.0) throw new RuntimeException(s"Error: failed to find ($a,$b) in table")

      d(i)(j) = min(
        d(i - 1)(j) + 1, // deletion
        d(i)(j - 1) + 1, // insertion
        d(i - 1)(j - 1) + cost // substitution
      )
    }

    d(lenStr1)(lenStr2)
  }

  def min(nums: Double*): Double = nums.min


  /**
   * Methods to Process Info
   **/

  // Decodes the unicode characters for looking up in the other format similarity table
  def decode(in: String): String = {
    if (in == "ʒ") return "S"
    if (in == "ʔ") return "?"
    if (in == "ɛ") return "E"
    if (in == "ɔ") return "c"
    if (in == "ʊ") return "U"
    if (in == "ħ") return "h"
    if (in == "ɐ") return "A"
    if (in == "ʃ") return "S"
    if (in == "ɪ") return "I"

    in
  }

  /**
   * Scoring Methods
   **/

  def dhScore(in: LexicalItem, gang: Gang): Double = {
    var score: Double = Double.MinValue
    // todo:DO
    score
  }

  /**
   * Loading/Saving Methods
   **/

  def loadSimilarities(filename: String): HashMap[(String, String), Double] = {
    val out = new HashMap[(String, String), Double]

    // Load
    println("Loading data from " + filename)
    val source = scala.io.Source.fromFile(filename, "UTF8")
    val lines = source.getLines().toArray

    for (i <- 1 until lines.length) {
      val line = lines(i).trim()
      val data = line.split("\t")
      assert(data.length == 5)
      val a = data(0)
      val b = data(1)
      val sim = data(4).toDouble
      out((a, b)) = sim
    }

    out
  }


  def main(args:Array[String]): Unit = {

    val similarityTableFile = "/home/becky/Documents/maltesePhonemeFeatures.stb"
    val table = loadSimilarities(similarityTableFile)

    val s1 = "dʔzɛ"
    val s2 = "dʔsɛ"

    println("Weighted Dist: " + weightedLevenshtein(s1, s2, table))
  }

}
