package brokenPlurals

import scala.collection.mutable.{ArrayBuffer, Set, HashMap}

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
      val cost: Double = if (a == b) 0 else table.getOrElse((a,b), table.getOrElse((b,a), -1.0))

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
    if (in == "ʊ" || in == "u") return "U"
    if (in == "ħ") return "h"
    if (in == "ɐ") return "A"
    if (in == "ʃ") return "S"
    if (in == "ɪ") return "I"

    in
  }

  // Uses the given CV Templates to find the vowel characters
  def extractVowels (a:Array[String], b:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]
    for (i <- 0 until a.length){
      if (b(i) == "V") out.append(a(i))
    }
    out.toArray
  }

  def fixTrans(in:String):String = {
    var split = in.split("")
    if (split.head == "") split = split.slice(1,1000)
    val s1 = removeApostrophe(split)
    val s2 = fixAffricates(s1)
    val s3 = fixLongVowels(s2)

    s3.mkString("")
  }

  def fixLongVowels(in:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]
    for (i <- 0 until in.length) {
      if (in(i) == "ː") out.append (in(i - 1))
      else out.append(in(i))
    }
    out.toArray
  }

  // Removes the apostrophes in the transcriptions
  def removeApostrophe (in:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]
    for (char <- in) if (!Array("ˈ", "'").contains(char)) out.append(char)
    out.toArray
  }

  // Combines affricates into one character
  def fixAffricates(in:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]
    var append = true
    for (i <- 0 until in.length - 1) {
      val char = in(i)
      val next = in(i + 1)
      //ts, tʃ, or dz
      if (char == "t" && next == "s") {
        out.append("T")
//        println ("\tcurr: " + out.mkString(","))
        append = false
      } else if (char == "t" && next == "ʃ") {
        out.append("C")
//        println ("\tcurr: " + out.mkString(","))
        append = false
      } else if (char == "d" && next == "z") {
        out.append("D")
//        println ("\tcurr: " + out.mkString(","))
        append = false
      } else if (char == "d" && next == "ʒ") {
        out.append("J")
//        println ("\tcurr: " + out.mkString(","))
        append = false
      }
      else if (append == true) {
        out.append(char)
//        println ("\tcurr: " + out.mkString(","))
      } else if (append == false && i != in.length - 1) {   // reset for the next character
        append = true
      }

    }
    if (append){
      out.append(in(in.length - 1))
//      println ("\tcurr: " + out.mkString(","))
    }

    out.toArray
  }

  // Generates the CV templates for the singular forms of the lexical items
  def generateSingularTemplates (in:Array[LexicalItem], vowels:Array[String]): Unit = {
    for (li <- in) {
//      println("\n---------------------------------------------------------------------\n")
      li.cvTemplateSgTrans = makeCVTemplate(li.sgTrans, vowels)
      //println ("SgTrans: " + li.sgTrans + "\tCVTemp: " + li.cvTemplateSgTrans)

      // Checking for inconsistencies
      val tempCVPlural = makeCVTemplate(li.plTrans, vowels)
      if (tempCVPlural != li.cvTemplatePlTrans) {
        println ("plTrans: " + li.plTrans)
        println ("Provided Plural Template: " + li.cvTemplatePlTrans)
        println ("  Generated: " + tempCVPlural)
        sys.exit(1)
      }
    }
  }

  // Given a phonemic transcriptions, generate a CV template
  def makeCVTemplate (trans:String, vowels:Array[String]):String = {
    val out = new ArrayBuffer[String]
    val split = trans.split("").slice(1, 1000)
    val fixed = fixAffricates(removeApostrophe(split))
//    println ("trans: " + trans)
//    println ("fixed split: " + fixed.mkString(", "))
    for (char <- fixed) {
      if (vowels.contains(char)) out.append("V")
      else  out.append("C")
    }
    val outString = out.mkString("")
//    println ("outString: " + outString)
    assert (outString.length == fixed.length)

    outString
  }

  /**
   * Scoring Methods
   **/

  // Calculates the similarity between two lexical forms, based on the similarity equation in Dawdy-Hesterberg&Pierrehumbert 2014
  //  NOTE: the default values here for s and p are the ones used in the paper, but can be tuned as well
  def dhSimilarity(i: LexicalItem, j:LexicalItem, table:HashMap[(String,String), Double], s:Double = 0.3, p:Double = 1.0): Double = {
    // DH-PH 2014 notation ==> n_ij = exp(-d_ij/s)^p
    // TODO: check?
    Math.pow( Math.exp( - weightedLevenshtein(i.sgTrans, j.sgTrans, table) / s), p)
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


  /**
   * Debug Methods
   **/
  def printAllPhones(in:Array[LexicalItem]): Unit = {
    val allPhones = Set[String]()
    for (li <- in) {
      val currPhones = (li.plTrans++li.sgTrans).split("")
      for (char <- currPhones) allPhones.add(char)
    }
    println ("All Phones:")
    allPhones.toArray.foreach(println(_))
  }

  def main(args:Array[String]): Unit = {

    val similarityTableFile = "/home/becky/Documents/maltesePhonemeFeatures.stb"
    val table = loadSimilarities(similarityTableFile)

    val s1 = "dʔzɛ"
    val s2 = "dʔsɛ"

    println("Weighted Dist: " + weightedLevenshtein(s1, s2, table))
  }

}
