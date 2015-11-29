package brokenPlurals

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer,Set}
import scala.io.Source

/**
 * Created by becky on 11/28/15.
 */
class BrokenPlurals {

}

object BrokenPlurals {

  // Loads the broken_plural.csv file from the online corpus resources
  def loadCSV (filename:String):Array[LexicalItem] = {
    val out = new ArrayBuffer[LexicalItem]

    // Load
    println ("Loading data from " + filename)
    val source = scala.io.Source.fromFile(filename, "UTF8")
    val lines = source.getLines()


    for (line <- lines.slice(1, 1000)) {
      println (line)
      val data = line.trim().split("\t")
      assert (data.length == 8)

      val sgOrth = data(0)
      val plOrth = data(1)
      val sgTrans = data(2)
      val plTrans = data(3)
      val gender = data(4)
      val gloss = data(5)
      val oldType = if (data(6) != "") data(6).toInt else -1
      val cvTemplatepluralTrans = data(7)

      out.append(new LexicalItem(sgOrth, plOrth, sgTrans, plTrans, gender, gloss, oldType, cvTemplatepluralTrans))

    }

    println ("Finished loading, " + out.length + " pairs found.")
    out.toArray
  }

  def makeGangs (in:Array[LexicalItem]):Array[Gang] = {
    val out = new ArrayBuffer[Gang]

    //todo: DO!

    out.toArray
  }

  def makeVowelSet (in:Array[LexicalItem]):Array[String] = {
    val vowels = Set[String]()
    for (lexicalItem <- in) {
      val template = lexicalItem.cvTemplatePlTrans.split("").slice(1,1000)
      val plTransRaw = lexicalItem.plTrans.split("")

      // remove ' and ˈ (different characters used)
      val plTrans = removeApostrophe(plTransRaw.slice(1,1000))

      // Fix affricates
      val plTrans1 = fixAffricates(plTrans)

      // Other?
      if (template.length == plTrans1.length) {
        // Extract the vowels
        val vowelsFromCurr = extractVowels(plTrans1, template)
        for (v <- vowelsFromCurr) {
          vowels.add(v)
        }
      } else{
        // ERROR: Length Mismatch
        println ("Not included: ")
        println ("\toriginal: " + plTransRaw.mkString(",") + s"\tfixedPlTrans: ${plTrans1.mkString(",")}\t(len ${plTrans1.length})" +
          s"\tCVTemplate: ${template.mkString(",")}\t(len ${template.length})")
      }

    }
    println ("\nExtracted Vowels: ")
    vowels.foreach(println(_))
    vowels.toArray
  }

  def extractVowels (a:Array[String], b:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]
    for (i <- 0 until a.length){
      if (b(i) == "V") out.append(a(i))
    }
    out.toArray
  }

  def removeApostrophe (in:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]
    for (char <- in) if (!Array("ˈ", "'").contains(char)) out.append(char)
    out.toArray
  }

  def fixAffricates(in:Array[String]):Array[String] = {
    val out = new ArrayBuffer[String]
    var append = true
    for (i <- 0 until in.length - 1) {
      val char = in(i)
      val next = in(i + 1)
      //ts, tʃ, or dz
      if (char == "t" && next == "s") {
        out.append("T")
        append = false
      } else if (char == "t" && next == "ʃ") {
        out.append("C")
        append = false
      } else if (char == "d" && next == "z") {
        out.append("D")
        append = false
      } else if (char == "d" && next == "ʒ") {
        out.append("J")
        append = false
      }
      else if (append == true) {
        out.append(char)
      } else if (append == false && i != in.length - 2) {   // reset for the next character
        append = true
      }

    }
    if (append) out.append(in(in.length - 1))

    out.toArray
  }


  def main(args:Array[String]) {
    // Load the Singular-Plural pairs
    val lexicalItems = loadCSV("/home/becky/Downloads/broken_plural.csv")

    // Make a set of the vowels (for generating CV templates of other forms)
    val vowelSet = makeVowelSet(lexicalItems)

    // Generate CVTemplates for the singular forms

    // Assign Gangs to each item

    // split the data into folds

    // for each fold:

    // for each item:

    // find the closest gang

    // determine accuracy
  }
}
