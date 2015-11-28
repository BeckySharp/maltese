package brokenPlurals

import scala.collection.immutable.HashMap

/**
 * Created by becky on 11/28/15.
 */
class BPUtils {

  /**
   * Methods to Calculate Distances
   **/

  // From https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Scala
  // modified to use a weighted distance
  def weightedLevenshtein(str1: String, str2: String, table:HashMap[(String,String),Double]): Double = {
    val lenStr1 = str1.length
    val lenStr2 = str2.length

    val d: Array[Array[Double]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val cost:Double = if (str1(i - 1) == str2(j-1)) 0 else table.get((str1(i - 1).toString, str2(j-1).toString)).getOrElse(-1.0)
      if (cost == -1.0) throw new RuntimeException (s"Error: failed to find (${str1(i - 1)},${str2(j-1)}) in table")

      d(i)(j) = min(
        d(i-1)(j  ) + 1,     // deletion
        d(i  )(j-1) + 1,     // insertion
        d(i-1)(j-1) + cost   // substitution
      )
    }

    d(lenStr1)(lenStr2)
  }

  def min(nums: Double*): Double = nums.min



  /**
   * Methods to Process Info
   **/


  /**
   * Scoring Methods
   **/

  def dhScore(in:LexicalItem, gang:Gang):Double = {
    var score:Double = Double.MinValue
    // todo:DO
    score
  }

}
