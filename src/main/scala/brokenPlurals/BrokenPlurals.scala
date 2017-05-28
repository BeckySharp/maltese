package brokenPlurals

//import Structs.{Counter, Lexicon}

import java.io.PrintWriter

import edu.arizona.sista.struct.{Counter, Lexicon}

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.{ArrayBuffer,Set}
import scala.io.Source
import scala.util.Random

/**
 * Created by becky on 11/28/15.
 */

object BrokenPlurals {

  def doCrossValidationClassification(classifier:Classifier,
                                      classicFolds:Array[ClassicFold],
                                      table:HashMap[(String, String), Double],
                                      k:Int = 0,
                                      testOn:String = "DEV",
                                      restricted:Boolean,
                                      gangLexicon: Lexicon[String]): (Array[Double], Double, Array[Double]) = {
    // Initialize
    val numFolds = classicFolds.length
    val foldAccuracies = new Array[Double](numFolds)
    val allResults = new ArrayBuffer[Double]
    val chancePerformances = new Array[Double](numFolds)
    // For F1 stats
    val truePositives = new Array[Double](gangLexicon.size)
    val falseNegatives = new Array[Double](gangLexicon.size)
    val falsePositives = new Array[Double](gangLexicon.size)

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
      val trainingItems = new ArrayBuffer[LexicalItem]
      //val trainingFolds = new ArrayBuffer[Int]
      for (j <- 0 until numFolds) {
        //if (j != testingFold && j != leftOutFold) trainingFolds.append(j)
        if (j != testingFold && j != leftOutFold) {
          trainingGangs.insertAll(trainingGangs.length, classicFolds(j).foldGangs)
          trainingItems.insertAll(trainingItems.length, classicFolds(j).foldData)
        }
      }

      val mergedTrainingGangs = Gang.mergeGangs(trainingGangs).toArray

      // Check the ceiling performance:
      val (ceiling, chancePerformance) = checkCeiling(testingItems.toArray, trainingGangs.toArray)
      //val (ceiling, chancePerformance) = checkCeiling(testingItems.toArray, mergedTrainingGangs)
      chancePerformances(i) = chancePerformance
      println (s"** Fold $i Ceiling: $ceiling \t Chance Performance =  $chancePerformance")


      // Step 2: Classify
      val allItems = new ArrayBuffer[LexicalItem]
      allItems.insertAll(0,trainingItems)
      allItems.insertAll(allItems.length, testingItems)
      //val classifications = classifier.classify(mergedTrainingGangs, allItems.toArray, table, k, trainingItems.length, restricted)
      val classifications = classifier.classify(trainingGangs.toArray, allItems.toArray, table, k, trainingItems.length, restricted)

      // Step 3: Evaluate
      val (accuracy, foldResults) = evaluate(testingItems.toArray, classifications)
      val (foldTP, foldFP, foldFN) = evaluateByGang(testingItems.toArray, classifications, gangLexicon)
      BPUtils.addArrayInPlace(truePositives, foldTP)
      BPUtils.addArrayInPlace(falsePositives, foldFP)
      BPUtils.addArrayInPlace(falseNegatives, foldFN)


      // Step 4: Store Accuracy
      foldAccuracies(testingFold) = accuracy

      // Step 5: Store Results
      allResults.insertAll(allResults.length, foldResults)

      //      println ("tr: " + trainingFolds.mkString(""))
//      println ("testing on: " + testingFold)
//      println ("leftout: " + leftOutFold)
    }

    println ("Final Chance performance: " + chancePerformances.sum)

    // Display the final TP, FP, and FNs for each gang
    println (s"Of ${gangLexicon.size} gangs, these were the TP, FP, and FN stats:")
//    println ("GangTemplate\tTP\tFP\tFN")
//    for (i <- 0 until gangLexicon.size) {
//      println (s"${gangLexicon.get(i)}\t${truePositives(i)}\t${falsePositives(i)}\t${falseNegatives(i)}")
//    }

    displayClassF1(gangLexicon, truePositives, falsePositives, falseNegatives)
    // Calculate the macro/micro F1
    val microF1 = calcMicroF1(truePositives, falsePositives, falseNegatives)
    val macroF1 = calcMacroF1(truePositives, falsePositives, falseNegatives)
    println (s"The F1 scores are:\tmacro(averaged across all) = $macroF1  \tmicro(summed) = $microF1")

    // Return all fold accuracies and the average accuracy and all results
    val avgAcc:Double = foldAccuracies.sum.toDouble / numFolds.toDouble
    (foldAccuracies, avgAcc, allResults.toArray)
  }

  def checkCeiling (items:Array[LexicalItem], gangs:Array[Gang]):(Double, Double) = {
    val itemsCounter = new Counter[String]
    for (item <- items) {
      itemsCounter.incrementCount(item.cvTemplateSgTrans)
    }

    val choicesCounter = new Counter[String]
    for (gang <- gangs) {
      val singularForm = gang.getSingular()
      choicesCounter.incrementCount(singularForm)
    }

    val allTestItemsTemplates = itemsCounter.keySet
    val allTemplateChoices = choicesCounter.keySet

    val testingTemplatesSeenInTraining = allTestItemsTemplates.intersect(allTemplateChoices)
    val ceiling:Double = testingTemplatesSeenInTraining.size.toDouble / allTestItemsTemplates.size.toDouble

    println (s"** Of ${allTestItemsTemplates.size} templates seen in testing, ${testingTemplatesSeenInTraining.size} were available for classification...")


    // Calculate Chance performance
    var chancePerformance:Double = 0.0
    for (item <- items) {
      var optionsCounter:Double = 0.0
      for (gang <- gangs) {
        val sgForm = gang.getSingular()
        if (item.cvTemplateSgTrans == sgForm) optionsCounter += 1.0
      }
      chancePerformance += 1.0 / optionsCounter
    }

    (ceiling, chancePerformance / items.length)
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
      counter.incrementCount(gangString)
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
      val count = counter.getCount(g.gangString)
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

  // Returns both the accuracy for the fold (Double) as well as an array of indicators as to whether a given question was correct
  // or not (for use with statistical post processing)
  def evaluate (items:Array[LexicalItem], classifications:Array[(String, Int)]):(Double, Array[Double]) = {

    var nCorrect:Double = 0.0
    val nItems = items.length
    val resultsByQuestion = new Array[Double](nItems)

    for (iIdx <- 0 until nItems) {
      val correct = items(iIdx).gangString
      if (classifications(iIdx)._1 == correct) {
        val questionAccuracy = 1.0 / classifications(iIdx)._2.toDouble
        nCorrect += questionAccuracy
        resultsByQuestion(iIdx) = questionAccuracy
      } else resultsByQuestion(iIdx) = 0

    }

    val accuracy = nCorrect / nItems.toDouble
    println (s"Of $nItems items, $nCorrect classified correctly (with ties handled)")
    println (s"\tFold Accuracy: $accuracy")

    (accuracy, resultsByQuestion)
  }

  // Returns both the accuracy for the fold (Double) as well as an array of indicators as to whether a given question was correct
  // or not (for use with statistical post processing)
  def evaluateByGang (items:Array[LexicalItem],
                      classifications:Array[(String, Int)],
                      gangLexicon: Lexicon[String]):(Array[Double], Array[Double], Array[Double]) = {

    val truePositives = new Array[Double](gangLexicon.size)
    val falseNegatives = new Array[Double](gangLexicon.size)
    val falsePositives = new Array[Double](gangLexicon.size)

    val nItems = items.length

    for (iIdx <- 0 until nItems) {
      val correct = items(iIdx).gangString
      val correctGangIndex = gangLexicon.get(correct).get
      val assignedGangIndex = gangLexicon.get(classifications(iIdx)._1).get

      if (classifications(iIdx)._1 == correct) {
        truePositives(assignedGangIndex) += 1.0
      } else {
        falsePositives(assignedGangIndex) += 1.0
        falseNegatives(correctGangIndex) += 1.0
      }
    }

//    println (s"Of ${gangLexicon.size} gangs, these were the TP, FP, and FN stats:")
//    for (i <- 0 until gangLexicon.size) {
//      println (s"  Gang ${gangLexicon.get(i)}: TP(${truePositives(i)}}) FP(${falsePositives(i)}}) FN(${falseNegatives(i)}})")
//    }

    (truePositives, falsePositives, falseNegatives)
  }

  def harmonicMean(a: Double, b:Double): Double = if (a + b == 0.0) 0.0 else (2.0 * a * b) / (a + b)

  def displayClassF1 (gangLexicon: Lexicon[String], truePositives: Array[Double],
                      falsePositives: Array[Double], falseNegatives: Array[Double]): Unit = {
    println ("\n-------------------------------------------------")
    println ("                F1 Scores by Gang")
    println ("-------------------------------------------------")
    println ("GangTemplate\tTruePos\tFalsePos\tFalseNeg\tPrecision\tRecall\tF1")
    for (i <- 0 until gangLexicon.size) {
      val (precision, recall, f1) = calcPRF1(truePositives(i), falsePositives(i), falseNegatives(i))
      println (s"${gangLexicon.get(i)}\t${truePositives(i)}\t${falsePositives(i)}\t${falseNegatives(i)}\t$precision\t$recall\t$f1")
    }
    println ("-------------------------------------------------")

  }

  def calcPRF1(truePositives: Double, falsePositives: Double, falseNegatives: Double): (Double, Double, Double) = {
    val precision = if (truePositives + falsePositives == 0) 0.0 else truePositives / (truePositives + falsePositives)
    val recall = if (truePositives + falseNegatives == 0) 0.0 else truePositives / (truePositives + falseNegatives)
    (precision, recall, harmonicMean(precision, recall))
  }

  def calcMacroF1(truePositives: Array[Double], falsePositives: Array[Double], falseNegatives: Array[Double]): Double = {
    val nClasses = truePositives.length

    // Find the precisions and recalls for each class
    val precisions = new Array[Double](nClasses)
    val recalls = new Array[Double](nClasses)
    for (i <- 0 until nClasses) {
      if (truePositives(i) + falsePositives(i) == 0.0) {
        precisions(i) = 0.0
      } else {
        precisions(i) = truePositives(i) / (truePositives(i) + falsePositives(i))
      }
      if (truePositives(i) + falseNegatives(i) == 0.0) {
        recalls(i) = 0.0
      } else {
        recalls(i) = truePositives(i) / (truePositives(i) + falseNegatives(i))
      }
    }
    // Average them
    val averagePrecision = precisions.sum / nClasses.toDouble
    val averageRecall = recalls.sum / nClasses.toDouble

    // Harmonic Mean
    harmonicMean(averagePrecision, averageRecall)
  }

  def calcMicroF1(truePositives: Array[Double], falsePositives: Array[Double], falseNegatives: Array[Double]): Double = {
    // Sum truePositives
    val truePositiveSum = truePositives.sum

    // Sum falsePositives
    val falsePositiveSum = falsePositives.sum

    // Sum falseNegatives
    val falseNegativeSum = falseNegatives.sum

    // Find the overall Precision
    val precision = truePositiveSum / (truePositiveSum + falsePositiveSum)

    // Find the overall Recall
    val recall = truePositiveSum / (truePositiveSum + falseNegativeSum)

    // Harmonic Mean
    harmonicMean(precision, recall)
  }

  // Run stats using bootstrap resampling to determine the p-value
  def runStats(a:Array[Double], b:Array[Double], nSamples:Int, randomSeed:Int = 426):Double = {
    val randA = new Random(randomSeed)
    val randB = new Random(randomSeed + 1)
    val nItems = a.length
    assert (b.length == nItems)

    var aBetter:Double = 0.0

    for (i <- 0 until nSamples) {
      var aSample:Double = 0.0
      var bSample:Double = 0.0

      // Generate a sample from both inputs
      for (j <- 0 until nItems) {
        val aIndex = randA.nextInt(nItems)
        val bIndex = randB.nextInt(nItems)
        aSample += a(aIndex)
        bSample += b(bIndex)
      }

      // Determine which is higher
      if (aSample > bSample) aBetter += 1.0
      if (aSample == bSample) {
        println ("Eek! a tie in the sampling!")
        aBetter += 0.5
      }
    }

    aBetter / nSamples.toDouble
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

    // todo - check lev dist! 1/sim?

    // Assign Gangs to each item
    val (gangs, gangLexicon, gangCounter) = makeGangs(lexicalItems)
    val (filteredGangs, filteredLexicon) = filterGangs(gangs, gangCounter, threshold = 4)

//    val pwShiloh1 = new PrintWriter("cvTemplatesSing_filt4.txt")
////    val pwShiloh2 = new PrintWriter("cvTemplatesPlural_flt5.txt")
//    var singSet = new ArrayBuffer[String]
////    var plurSet = new ArrayBuffer[String]
//    for (g <- filteredGangs) {
//      val cv = g.gangString.split("-")
//      singSet += cv(0)
////      plurSet += cv(1)
//    }
//    val set = singSet.toSet
//    for (item <- set) {
//      pwShiloh1.println(item)
//    }
//    pwShiloh1.close()
////    pwShiloh2.close()
//    sys.exit(0)

    // split the data into folds
    val numFolds:Int = 5
    val numTrials:Int = 1
    //val testOn = "DEV"
    val testOn = "TEST"
    //val trialFolds = makeFolds(filteredGangs, filteredLexicon, numFolds, numTrials)
    val trialFolds = makeClassicFolds(filteredGangs, filteredLexicon, numFolds, numTrials)
    //val trialAccuracies = new Array[Double](numTrials)


    // ----------------------------------------------------------------------------------------------------
    //  Classification Method 1
    // ----------------------------------------------------------------------------------------------------

    //val classifierMethod1 = DHPH2014_GCM
    //val classifierMethod1 = DHPH2014_restrictedGCM
    //val classifierMethod1 = kNearestNeighbors
    val classifierMethod1 = LogisticRegression
//    val restricted1:Boolean = false
    val restricted1:Boolean = true

    println (s"There are ${gangLexicon.size} gangs!")
    println (s"There are ${filteredLexicon.size} filtered gangs!")

    //sys.exit(0)

    val (accuracies1, avgAcc1, results1) = doCrossValidationClassification(
      classifierMethod1,
      trialFolds(0),
      similarityTable,
      k = 5,
      testOn,
      restricted1,
      filteredLexicon
    )

    // Display:
    println ("\n=================================================================================================================")
    println (s"\tclassifier 1: ${classifierMethod1.mkString()}\n\tnumFolds: $numFolds")
    println ("=================================================================================================================")
    println ("                                        Final Results - testing on: " + testOn)
    println ("=================================================================================================================\n")
    println ("Accuracy across all folds: " + accuracies1.mkString("\t"))
    println ("")
    println ("Final (Averaged) accuracy): " + avgAcc1)

    // ----------------------------------------------------------------------------------------------------
    //  Classification Method 2
    // ----------------------------------------------------------------------------------------------------

    sys.exit(1)
    //val classifierMethod2 = DHPH2014_GCM
    //val classifierMethod2 = DHPH2014_restrictedGCM
    //val classifierMethod2 = kNearestNeighbors
    val classifierMethod2 = LogisticRegression
    val restricted2:Boolean = false

    val (accuracies2, avgAcc2, results2) = doCrossValidationClassification(
      classifierMethod2,
      trialFolds(0),
      similarityTable,
      k = 5,
      testOn,
      restricted2,
      filteredLexicon
    )

    // Display:
    println ("\n=================================================================================================================")
    println (s"\tclassifier 2: ${classifierMethod2.mkString()}\n\tnumFolds: $numFolds")
    println ("=================================================================================================================")
    println ("                                        Final Results - testing on: " + testOn)
    println ("=================================================================================================================\n")
    println ("Accuracy across all folds: " + accuracies2.mkString("\t"))
    println ("")
    println ("Final (Averaged) accuracy): " + avgAcc2)


    // Statistical Analysis
    val nSamples = 10000
    val pValue = runStats(results1, results2, nSamples, randomSeed = 6)
    // Display:
    println ("\n\n=================================================================================================================")
    println (s"\tp-value: $pValue \t (using $nSamples samples)")
    println ("=================================================================================================================")



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