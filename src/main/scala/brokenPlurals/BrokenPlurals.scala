package brokenPlurals

import Structs.{Counter, Lexicon}

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer,Set}
import scala.io.Source
import scala.util.Random

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

  def makeGangs (in:Array[LexicalItem]):(Array[Gang], Lexicon[String], Counter[String]) = {
    val lexicon = new Lexicon[String]
    val counter = new Counter[String]

    //todo: DO!
    // Make the lexicon of possible gangs, assigning gang indices to the Lexical Items as you go
    for (li <- in) {
      val gangString = s"[${li.cvTemplateSgTrans}-${li.cvTemplatePlTrans}]"
      li.gang = lexicon.add(gangString)
      li.gangString = gangString
      counter.add(gangString)
    }

    // Iterate through the LIs and make the array of Gangs
    // Initialize:
    val gangs = new Array[Gang](lexicon.size)
    for (i <- 0 until gangs.length) gangs(i) = new Gang("")

    // Make Gangs:
    for (i <- 0 until in.length) {
      val li = in(i)
      val g = li.gang
      // If the first of this type, initialize
      if (gangs(g).size() == 0) gangs(g).gangString = li.gangString
      gangs(g).add(li)
    }

    (gangs, lexicon, counter)
  }

  def filterGangs (in:Array[Gang], counter:Counter[String], threshold:Int = 2):(Array[Gang], Lexicon[String]) = {
    val lexicon = new Lexicon[String]
    val out = new ArrayBuffer[Gang]

    for (g <- in) {
      val count = counter.getOrElse(g.gangString, -1)
      if (count == -1) throw new RuntimeException ("Error: gs " + g.gangString + " not found!")
      else if (count >= threshold) {
        println ("Using gang with sg-pl pattern: " + g.gangString)
        out.append(g)
        val indexAssigned = lexicon.add(g.gangString)
        assert (indexAssigned == out.size - 1)
      }
    }

    var itemCounter:Int = 0
    for (g <- out) itemCounter += g.size()

    println ("\n\nAfter Filtering with threshold of " + threshold + ", " + out.length + " gangs kept, with a total of " + itemCounter + " items.")
    (out.toArray, lexicon)
  }

  def makeFolds (in:Array[Gang], lexicon:Lexicon[String], numFolds:Int, numTrials:Int):Array[Fold] = {
    val trialFolds = new Array[Fold](numTrials)

    for (trial <- 0 until numTrials) {
      val random = new Random(trial)

      val testingItems = new ArrayBuffer[LexicalItem]
      val trainingItems = new ArrayBuffer[LexicalItem]

      // for each gang, retrieve the lexical items
      for (gang <- in) {
        val items = gang.members.toList

        // shuffle those lexical items
        val shuffled = random.shuffle(items).toArray

        // distribute into the folds, handling the overflow
        for (i <- 0 until shuffled.length) {
          if ((i + 1) % numFolds == 0) testingItems.append(shuffled(i)) // testing
          else if (i > (shuffled.length - (shuffled.length % numFolds) - 1)) {
            // Randomly assign the "leftovers"
            val rand = random.nextDouble()
            if (rand < 1.0/numFolds.toDouble) testingItems.append(shuffled(i))
            else trainingItems.append(shuffled(i))
          }
          else trainingItems.append(shuffled(i))  // training
        }
      }

      // Display
      println ("\nTrial: " + trial)
      println ("Number of items in training: " + trainingItems.length)
      println ("Number of items in testing: " + testingItems.length)

      // Find the gangs from the training data
      val trainingGangs = makeGangs(trainingItems.toArray)._1

      // Store the trial fold data
      trialFolds(trial) = new Fold(trainingGangs, testingItems.toArray)
    }

    trialFolds
  }

  def makeVowelSet (in:Array[LexicalItem]):Array[String] = {
    val vowels = Set[String]()
    for (lexicalItem <- in) {
      val template = lexicalItem.cvTemplatePlTrans.split("").slice(1,1000)
      val plTransRaw = lexicalItem.plTrans.split("")

      // remove ' and ˈ (different characters used)
      val plTrans = BPUtils.removeApostrophe(plTransRaw.slice(1,1000))

      // Fix affricates
      val plTrans1 = BPUtils.fixAffricates(plTrans)

      // Other?
      if (template.length == plTrans1.length) {
        // Extract the vowels
        val vowelsFromCurr = BPUtils.extractVowels(plTrans1, template)
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





  def main(args:Array[String]) {
    // Load the Singular-Plural pairs
    val lexicalItems = loadCSV("/home/becky/Downloads/broken_plural.csv")

    // Make a set of the vowels (for generating CV templates of other forms)
    val vowelSet = makeVowelSet(lexicalItems)

    // Generate CVTemplates for the singular forms
    BPUtils.generateSingularTemplates(lexicalItems, vowelSet)

    // Assign Gangs to each item
    val (gangs, gangLexicon, gangCounter) = makeGangs(lexicalItems)
    val (filteredGangs, filteredLexicon) = filterGangs(gangs, gangCounter, threshold = 2)

    // split the data into folds
    makeFolds(filteredGangs, filteredLexicon, numFolds = 4, numTrials = 3)

    // for each fold:

    // for each item:

    // find the closest gang

    // determine accuracy
  }
}

case class Fold(val trainingGangs:Array[Gang], testingData:Array[LexicalItem])
