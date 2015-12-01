package brokenPlurals

import Structs.{Counter, Lexicon}

import scala.collection.mutable.HashMap
import scala.collection.mutable.{ArrayBuffer,Set}
import scala.io.Source
import scala.util.Random

/**
 * Created by becky on 11/28/15.
 */

object BrokenPlurals {

  def doCrossValidationClassification(classifier:Classifier, classicFolds:Array[ClassicFold], table:HashMap[(String, String), Double],
                                      k:Int = 0, testOn:String = "DEV"): (Array[Double], Double) = {
    // Initialize
    val numFolds = classicFolds.length
    val foldAccuracies = new Array[Double](numFolds)

    // Set up the CV
    for (i <- 0 until numFolds) {
      // For each fold:
      // Step 1: Determine the trainingGangs and the test items (dev or test)
      // 1a: Determine testing fold and left-out fold
      val testingFold = i
      var leftOutFold = if (testOn == "DEV") i + 1 else i - 1
      if (leftOutFold >= numFolds) leftOutFold = 0
      else if (leftOutFold <= 0) leftOutFold = numFolds - 1
      // 1b: Retrieve testing data
      val testingItems = classicFolds(i).foldData

      // 1c: Retrieve Training Gangs
      val trainingGangs = new ArrayBuffer[Gang]
      //val trainingFolds = new ArrayBuffer[Int]
      for (j <- 0 until numFolds) {
        //if (j != testingFold && j != leftOutFold) trainingFolds.append(j)
        if (j != testingFold && j != leftOutFold) trainingGangs.insertAll(trainingGangs.length, classicFolds(j).foldGangs)
      }

      // Step 2: Classify
      val classifications = classifier.classify(trainingGangs.toArray, testingItems.toArray, table, k)

      // Step 3: Evaluate
      val accuracy = evaluate(testingItems.toArray, classifications)

      // Step 4: Store Accuracy
      foldAccuracies(testingFold) = accuracy

      //      println ("tr: " + trainingFolds.mkString(""))
//      println ("testing on: " + testingFold)
//      println ("leftout: " + leftOutFold)
    }

    // Return all fold accuracies and the average accuracy
    val avgAcc:Double = foldAccuracies.sum.toDouble / numFolds.toDouble
    (foldAccuracies, avgAcc)
  }

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
      val sgTrans = BPUtils.fixTrans(data(2))
      val plTrans = BPUtils.fixTrans(data(3))
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

  def makeClassicFolds (in:Array[Gang], lexicon:Lexicon[String], numFolds:Int, numTrials:Int):Array[Array[ClassicFold]] = {
    // Initialize
    val trialFolds = new Array[Array[ClassicFold]](numTrials)
    for (i <- 0 until numTrials) {
      trialFolds(i) = new Array[ClassicFold](numFolds)
      for (j <- 0 until numFolds) trialFolds(i)(j) = new ClassicFold
    }

    for (trial <- 0 until numTrials) {
      val random = new Random(trial)

//      val testingItems = new ArrayBuffer[LexicalItem]
//      val trainingItems = new ArrayBuffer[LexicalItem]

      // for each gang, retrieve the lexical items
      var overflowGoesTo:Int = 0
      for (gang <- in) {
        val items = gang.members.toList
        val numBaseItemsPerFold: Int = Math.floor(items.length / numFolds.toDouble).toInt
        val overflowStartsAt: Int = numBaseItemsPerFold * numFolds
        println ("num total items in currentGang: " + items.length)
        println ("numBaseItemsPerFold: " + numBaseItemsPerFold)
        println ("overflow starts at: " + overflowStartsAt)

        // shuffle those lexical items
        val shuffled = random.shuffle(items).toArray

        // DIstribute the main portion of the items
        var startAt:Int = 0
        for (fIdx <- 0 until numFolds) {
          val foldItems = shuffled.slice(startAt, startAt + numBaseItemsPerFold)
          println ("Fold " + fIdx + ": baseFoldItemsLength = " + foldItems.length)
          startAt += numBaseItemsPerFold
          // Add in the first round of data
          trialFolds(trial)(fIdx).foldData.insertAll(trialFolds(trial)(fIdx).foldData.length, foldItems)
        }

        // Distribute the overflow items, trying to balance the folds
        for (itemIndex <- overflowStartsAt until items.length) {
          trialFolds(trial)(overflowGoesTo).foldData.append(items(itemIndex))
          overflowGoesTo += 1
          // Check to see if it has cycled back around to the first fold
          if (overflowGoesTo == numFolds) overflowGoesTo = 0
        }
      }

      // Make the gangs for each fold
      for (fold <- trialFolds(trial)) {
        fold.foldGangs = makeGangs(fold.foldData.toArray)._1
      }

      // Display
      println ("\nTrial: " + trial)
      for (i <- 0 until numFolds) {
        println (s"Fold $i -- Number of items: ${trialFolds(trial)(i).foldData.size}")
      }
    }

    trialFolds
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

  def evaluate (items:Array[LexicalItem], classifications:Array[(String, Int)]):Double = {

    var nCorrect:Double = 0.0
    val nItems = items.length

    for (iIdx <- 0 until nItems) {
      val correct = items(iIdx).gangString
      if (classifications(iIdx)._1 == correct) nCorrect += 1.0 / classifications(iIdx)._2.toDouble
    }

    val accuracy = nCorrect / nItems.toDouble
    println (s"Of $nItems items, $nCorrect classified correctly (with ties handled)")
    println (s"\tFold Accuracy: $accuracy")

    accuracy
  }





  def main(args:Array[String]) {
    // Load the Singular-Plural pairs
    val lexicalItems = loadCSV("/home/becky/Downloads/broken_plural.csv")

    // Load the Similarity table
    val similarityTableFile = "/home/becky/Documents/maltesePhonemeFeatures.stb"
    val similarityTable = BPUtils.loadSimilarities(similarityTableFile)

    // Make a set of the vowels (for generating CV templates of other forms)
    val vowelSet = makeVowelSet(lexicalItems)

    // Generate CVTemplates for the singular forms
    BPUtils.generateSingularTemplates(lexicalItems, vowelSet)

    // Assign Gangs to each item
    val (gangs, gangLexicon, gangCounter) = makeGangs(lexicalItems)
    val (filteredGangs, filteredLexicon) = filterGangs(gangs, gangCounter, threshold = 4)

    // split the data into folds
    val numFolds:Int = 5
    val numTrials:Int = 1
    val testOn = "DEV"
    //val trialFolds = makeFolds(filteredGangs, filteredLexicon, numFolds, numTrials)
    val trialFolds = makeClassicFolds(filteredGangs, filteredLexicon, numFolds, numTrials)
    val trialAccuracies = new Array[Double](numTrials)

    //val classifierMethod = DHPH2014_GCM
    //val classifierMethod = DHPH2014_restrictedGCM
    val classifierMethod = kNearestNeighbors
    val (accuracies, avgAcc) = doCrossValidationClassification(classifierMethod, trialFolds(0), similarityTable, k = 5, testOn)

    // Display:
    println ("\n=================================================================================================================")
    println (s"\tclassifier: ${classifierMethod.mkString()}\n\tnumFolds: $numFolds")
    println ("=================================================================================================================")
    println ("                                        Final Results - testing on: " + testOn)
    println ("=================================================================================================================\n")
    println ("Accuracy across all folds: " + accuracies.mkString("\t"))
    println ("")
    println ("Final (Averaged) accuracy): " + avgAcc)


//    // for each fold:
//    for (tIdx <- 0 until numTrials) {
//      val trial = trialFolds(tIdx)
//
//      // Assign each testing item a gang using current method
//      //val assignments = DHPH2014_GCM.classify(trial.trainingGangs, trial.testingData, similarityTable)
//      //val assignments = DHPH2014_restrictedGCM.classify(trial.trainingGangs, trial.testingData, similarityTable)
//      val assignments = kNearestNeighbors.classify(trial.trainingGangs, trial.testingData, similarityTable, k = 3)
//
//      // Evaluate accuracy
//      trialAccuracies(tIdx) = evaluate(trial.testingData, assignments)
//    }
//
//    println ("\n----------------------------------------------\n")
//    println(s"Average accuracy across $numTrials: ${trialAccuracies.sum / numTrials}")

  }
}

case class Fold(trainingGangs:Array[Gang], testingData:Array[LexicalItem])

case class ClassicFold () {
  var foldGangs = new Array[Gang](0)
  val foldData = new ArrayBuffer[LexicalItem]
}