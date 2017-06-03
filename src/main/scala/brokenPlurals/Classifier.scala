package brokenPlurals

//import Structs.{Lexicon, Counter}
import edu.arizona.sista.learning._
import edu.arizona.sista.struct.{Counter, Lexicon}

import scala.collection.mutable

//import scala.collection.mutable

//import edu.arizona.sista.processors
import scala.collection.mutable.{ArrayBuffer, HashMap}


/**
 * Created by becky on 11/29/15.
 */
trait Classifier {
  /** Returns Array of (best_gang_string, num_gangs_tied_for_best) --> one for each item being classified **/
  def classify
    (gangs:Array[Gang],
     items:Array[LexicalItem],
     table: HashMap[(String, String), Double],    // The phoneme-phoneme similarity table
     k:Int,                                       // for kNN
     n:Int,                                       // cutoff for where training data ends and test data begins
     restricted:Boolean
    ):Array[(String, Int)]

  /** Returns sorted Array of (gang_string, score) --> one for each item being classified **/
  def rank
  (
    gangs:Array[Gang],
    items:Array[LexicalItem],
    table: HashMap[(String, String), Double],    // The phoneme-phoneme similarity table
    k:Int,                                       // for kNN
    n:Int,                                       // cutoff for where training data ends and test data begins
    restricted:Boolean
  ):Array[Array[(String, Double)]]


  def mkString ():String

  def mkTrainTestSplit(items: Array[LexicalItem], n: Int): (Array[LexicalItem], Array[LexicalItem]) = {
    (items.slice(0,n), items.slice(n, items.length))
  }
}


object DHPH2014_GCM extends Classifier {

  def itemSimilarityToGangs(item: LexicalItem, gangs: Seq[Gang], table: HashMap[(String, String), Double]): Array[Double] = {
    val similarityToGangs = Array.fill[Double](gangs.length)(0.0)
    var allSimilarities:Double = 0.0

    // Find the similarity to each gang
    for (i <- 0 until gangs.length) {
      val gang = gangs(i)

      // Find and store the similarity to the gang member
      for (member <- gang.members) {
        val simToMember = BPUtils.dhSimilarity(item, member, table)
        similarityToGangs(i) += simToMember
        allSimilarities += simToMember
      }
    }
    // Store the similarities to all the gangs for this item
    //    These are the S_{i,C_j} values for each j in K (K is the set of all gangs) from DH-PH 2014
    val finalSimilarities = similarityToGangs.map(x => x / allSimilarities)

    finalSimilarities
  }

  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double], k:Int = 0, n:Int, restricted:Boolean):Array[(String, Int)] = {
    // Here - n is the cutoff saying where training data ends and testing data begins
    val (trainingItems, testingItems) = mkTrainTestSplit(items, n)

    val classifications = new Array[(String,Int)](testingItems.length)

    for (iIdx <- 0 until testingItems.length){

      val item = testingItems(iIdx)
      val finalSimilarities = itemSimilarityToGangs(item, gangs, table)

      // Find the best gang, handling ties
      var maxSim:Double = Double.MinValue
      var bestGang = new ArrayBuffer[Int]
      for (gIdx <- 0 until gangs.length) {
        if (finalSimilarities(gIdx) > maxSim) {
          maxSim = finalSimilarities(gIdx)
          bestGang = new ArrayBuffer[Int]
          bestGang.append(gIdx)
        } else if (finalSimilarities(gIdx) == maxSim) {
          bestGang.append(gIdx)
        }
      }

      // Store the classification as well as number of items tied for "best"
      classifications(iIdx) = (gangs(bestGang.head).gangString, bestGang.length)
    }

    classifications
  }

  def rank (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double],
            k:Int = 0, n:Int, restricted:Boolean):Array[Array[(String, Double)]] = {
    // Here - n is the cutoff saying where training data ends and testing data begins
    val (_, testingItems) = mkTrainTestSplit(items, n)

    val rankedGangsPerItem = for {
      (item, iIdx) <- testingItems.zipWithIndex
      gangSimilarities = itemSimilarityToGangs(item, gangs, table)
      gangSimilaritiesNamed = for {
        (gangSimilarity, gangIndex) <- gangSimilarities.zipWithIndex
        currGangString = gangs(gangIndex).gangString
      } yield (currGangString, gangSimilarity)
      sortedGangsForItem = gangSimilaritiesNamed.sortBy(- _._2)
    } yield sortedGangsForItem

    rankedGangsPerItem
  }

  def mkString():String = "Dawdy-Hesterberg & Pierrehumbert (2014) -- GCM"
}

object DHPH2014_restrictedGCM extends Classifier {

  def rank (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double],
            k:Int = 0, n:Int, restricted:Boolean):Array[Array[(String, Double)]] = {
    // Here - n is the cutoff saying where training data ends and testing data begins
    val (_, testingItems) = mkTrainTestSplit(items, n)

    val rankedGangsPerItem = for {
      (item, iIdx) <- testingItems.zipWithIndex
      (gangSimilarities, _) = itemSimilarityToGangs(item, gangs, table)
      gangSimilaritiesNamed = for {
        (gangSimilarity, gangIndex) <- gangSimilarities.zipWithIndex
        currGangString = gangs(gangIndex).gangString
      } yield (currGangString, gangSimilarity)
    // Only keep the ones that are not 0.0 (i.e. that match the singular template)
      sortedGangsForItem = gangSimilaritiesNamed.filter(_._2 != 0.0).sortBy(- _._2)
    } yield sortedGangsForItem

    rankedGangsPerItem
  }

  def itemSimilarityToGangs(item: LexicalItem, gangs: Seq[Gang], table: HashMap[(String, String), Double]): (Array[Double], Int) = {
    // Initialize variables for the fold
    var numCandGangs:Int = 0                      // Keeps Track of the eligible gangs for this item
    val similarityToGangs = Array.fill[Double](gangs.length)(0.0)
    var allSimilarities:Double = 0.0

    // Find the similarity to each gang
    for (i <- 0 until gangs.length) {
      val gang = gangs(i)

      // Find the singular for to restrict the candidate gangs
      val singularForm = gang.getSingular()

      // If the singular form of the gang matches the singular form of the item, add the similarities
      if (singularForm == item.cvTemplateSgTrans) {
        // Increment the candidate gangs counter
        numCandGangs += 1

        // Find and store the similarity to the gang member
        for (member <- gang.members) {
          val simToMember = BPUtils.dhSimilarity(item, member, table)
          similarityToGangs(i) += simToMember
          allSimilarities += simToMember
        }
      }
    }
    // Store the similarities to all the gangs for this item
    //    These are the S_{i,C_j} values for each j in K (K is the set of all gangs) from DH-PH 2014
    val finalSimilarities = similarityToGangs.map(x => x / allSimilarities)
    (finalSimilarities, numCandGangs)
  }

    def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double], k:Int = 0, n:Int, restricted:Boolean):Array[(String, Int)] = {
    // Here - n is the cutoff saying where training data ends and testing data begins
    val (trainingItems, testingItems) = mkTrainTestSplit(items, n)

    val classifications = new Array[(String,Int)](testingItems.length)
    val numItems = testingItems.length

    // Keep track of the number of gangs chosen between for post analysis
    var minNumCandGangs:Int = 10000
    var maxNumCandGangs:Int = -1
    var avgNumCandGangs:Double = -1.0

    for (iIdx <- 0 until numItems){

      val item = testingItems(iIdx)
      val (finalSimilarities, numCandGangs) = itemSimilarityToGangs(item, gangs, table)

      // Find the best gang, handling ties
      var maxSim:Double = Double.MinValue
      var bestGang = new ArrayBuffer[Int]
      for (gIdx <- 0 until gangs.length) {
        if (finalSimilarities(gIdx) > maxSim) {
          maxSim = finalSimilarities(gIdx)
          bestGang = new ArrayBuffer[Int]
          bestGang.append(gIdx)
        } else if (finalSimilarities(gIdx) == maxSim) {
          bestGang.append(gIdx)
        }
      }

      // Update the candidate gang trackers
      if (numCandGangs > maxNumCandGangs) maxNumCandGangs = numCandGangs
      if (numCandGangs < minNumCandGangs) minNumCandGangs = numCandGangs
      avgNumCandGangs += numCandGangs / numItems.toDouble

      // Store the classification as well as number of items tied for "best"
      classifications(iIdx) = (gangs(bestGang.head).gangString, bestGang.length)
    }

    //Display stats
    println ("Candidate Gang stats for the fold: ")
    println (s"Min: $minNumCandGangs\tmax: $maxNumCandGangs\tavg: $avgNumCandGangs")

    classifications
  }

  def mkString():String = "Dawdy-Hesterberg & Pierrehumbert (2014) -- Restricted GCM"
}

object kNearestNeighbors extends Classifier {

  def rank(gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double],
           k:Int = 0, n:Int, restricted:Boolean):Array[Array[(String, Double)]] = ???

  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double], k:Int, n:Int, restricted:Boolean):Array[(String, Int)] = {
    // Here - n is the cutoff saying where training data ends and testing data begins
    val (trainingItems, testingItems) = mkTrainTestSplit(items, n)

    val classifications = new Array[(String,Int)](testingItems.length)

    for (iIdx <- 0 until testingItems.length){

      val item = testingItems(iIdx)

      // Initialize variables
      val similarities = new ArrayBuffer[(Double, Int)]

//      val similarityToGangs = Array.fill[Double](gangs.length)(0.0)
//      var allSimilarities:Double = 0.0

      // Find the similarity to each gang
      for (i <- 0 until gangs.length) {
        //TODO: implement option where you limit by sing TEMPLATE
        val gang = gangs(i)

        // Find and store the similarity to the gang member
        for (member <- gang.members) {

          // Determine if we're adding
          var addTo:Boolean = true
          if (restricted) {
            if (item.cvTemplateSgTrans != gang.getSingular()) {
              addTo = false
            }
          }

          if (addTo) {
            val simToMember = BPUtils.dhSimilarity(item, member, table)
            similarities.append((simToMember, i))
          }

        }
      }


      // Sort the neighbors
      val sortedNeighbors = similarities.sortBy(- _._1)

      // Retrive the top k
      val topK = sortedNeighbors.slice(0, k)

      // Find the most common
      val gangCounter = new Counter[Int]
      for (j <- 0 until Math.min(k, topK.length)) gangCounter.incrementCount(topK(j)._2)

      // Check that some gangs occur more than once
      var maxGangs:Double = 1.0
      var modeGang:Int = -1
      for (k <- gangCounter.keySet) {
        val count = gangCounter.getCount(k)
        if (count > maxGangs) {
          maxGangs = count
          modeGang = k
        }
      }

      if (modeGang == -1) {
        println ("** no gangs occurred more than once, so highest match chosen")
        // Find number of items with highest score
        val topScore = sortedNeighbors.head._1
        val topGang = sortedNeighbors.head._2
        var nTies:Int = 0
        for (neighbor <- sortedNeighbors) {
          if (neighbor._1 == topScore) nTies += 1
        }
        if (nTies > 1) println ("**\tnTies: " + nTies)
        classifications(iIdx) = (gangs(topGang).gangString, nTies)
      } else {
        classifications(iIdx) = (gangs(modeGang).gangString, 1)
        // todo: do I need to handle ties here?
      }
    }


    classifications
  }

  def mkString():String = "KNN"
}

object LogisticRegression extends Classifier {

  val MIN_EXTENSION = "_MinSim"
  val MAX_EXTENSION = "_MaxSim"
  val AVG_EXTENSION = "_AvgSim"
  val SIZE_EXTENSION = "_size"
  val DHSIM_EXTENSION = "_newMinSim"

  // Used for splitting the items to Train, Test, and Rank split
  def getRankingSplit(items: Array[LexicalItem], n1: Int, n2: Int): (Array[LexicalItem], Array[LexicalItem], Array[LexicalItem]) = {
    (items.slice(0,n1), items.slice(n1, n2), items.slice(n2, items.length))
  }

  def rank (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double],
    k:Int = 0, n:Int, restricted:Boolean):Array[Array[(String, Double)]] = {
    val (trainingItems, testingItems) = mkTrainTestSplit(items, n)

    // Make a master feature lexicon
    val featureLexicon = new Lexicon[String]
    for (gangIndex <- 0 until gangs.length) {
      featureLexicon.add("g" + gangIndex + MIN_EXTENSION)
      featureLexicon.add("g" + gangIndex + MAX_EXTENSION)
      featureLexicon.add("g" + gangIndex + AVG_EXTENSION)
      featureLexicon.add("g" + gangIndex + SIZE_EXTENSION)
      featureLexicon.add("g" + gangIndex + DHSIM_EXTENSION)
    }

    // Make an RVFDataset for the training items
    val trainingDataset = makeDataset(trainingItems, gangs, featureLexicon, table, restricted)
    val scaleRange = Datasets.svmScaleRVFDataset(trainingDataset, 0.0, 1.0)

    // Initialize the Logistic Regression Classifier
    val classifier = new LogisticRegressionClassifier[String, Int](C = 1.0, bias = true)

    // Train
    classifier.train(trainingDataset)

    // Rank
    val rankedGangsPerItem = for {
      (testItem, i) <- testingItems.zipWithIndex
      datum = makeDatum(testItem, gangs, featureLexicon, table, restricted, rescale = true, scaleRange)
      distribution = classifier.scoresOf(datum).toSeq.sortBy(-_._2)
    } yield distribution.toArray

    rankedGangsPerItem
  }

  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double], k:Int, n:Int, restricted:Boolean):Array[(String, Int)] = {
    // Here - n is the cutoff saying where training data ends and testing data begins
    val (trainingItems, testingItems) = mkTrainTestSplit(items, n)

    // Make a master feature lexicon
    val featureLexicon = new Lexicon[String]
    for (gangIndex <- 0 until gangs.length) {
      featureLexicon.add("g" + gangIndex + MIN_EXTENSION)
      featureLexicon.add("g" + gangIndex + MAX_EXTENSION)
      featureLexicon.add("g" + gangIndex + AVG_EXTENSION)
      featureLexicon.add("g" + gangIndex + SIZE_EXTENSION)
      featureLexicon.add("g" + gangIndex + DHSIM_EXTENSION)
    }

    // Make an RVFDataset for the training items
    val trainingDataset = makeDataset(trainingItems, gangs, featureLexicon, table, restricted)
    val scaleRange = Datasets.svmScaleRVFDataset(trainingDataset, 0.0, 1.0)

    // Initialize the Logistic Regression Classifier
    val classifier = new LogisticRegressionClassifier[String, Int](C = 1.0, bias = true)

    // Train
    classifier.train(trainingDataset)
    //val weights = classifier.getWeights(verbose = true)

    // Display weights
//    println ("Weights: \n")
//    for (label <- weights.keySet) {
//      println ("Label: " + label)
//      println ("\tweights: ")
//      val classCounter = weights.get(label).get
//      for (index <- classCounter.keySet){
//        val featureName = featureLexicon.get(index)
//        println (s"f$index: $featureName -- ${classCounter.getCount(index)}")
//      }
//    }

    // Test
    var accuracy:Double = 0.0
    val classifications = new ArrayBuffer[(String, Int)]
    val confidencesCorrect = new ArrayBuffer[Double]
    val confidencesIncorrect = new ArrayBuffer[Double]

    for (i <- 0 until testingItems.length) {
      val testItem = testingItems(i)
      val datum = makeDatum (testItem, gangs, featureLexicon, table, restricted, rescale = true, scaleRange)

      val predictedLabel = classifier.classOf(datum)
      val distribution = classifier.scoresOf(datum)
      val confidence = distribution.getCount(predictedLabel)

      classifications.append((predictedLabel, 1))

      if (predictedLabel == testItem.gangString) {
        println ("Successful classification - " + predictedLabel)
        accuracy += 1.0 / testingItems.length
        confidencesCorrect.append(confidence)
      } else {
        println ("Failed classification - predicted: " + predictedLabel + "\tcorrect: " + testItem.gangString)
        confidencesIncorrect.append(confidence)
      }
    }

    println ("Accuracy: " + accuracy)
    println ("Average Confidence when CORRECT: " + confidencesCorrect.sum / confidencesCorrect.length.toDouble)
    println ("Average Confidence when INCORRECT: " + confidencesIncorrect.sum / confidencesIncorrect.length.toDouble)

    classifications.toArray
  }

  // Make an RVFDataset from LexicalItems and Gangs
  def makeDataset(items:Array[LexicalItem], gangs:Array[Gang], featureLexicon:Lexicon[String],
                  table:HashMap[(String, String), Double], restricted:Boolean = false, rescale:Boolean = false,
                  scaleRange:ScaleRange[Int] = new ScaleRange[Int]):RVFDataset[String,Int] = {
    val dataset = new RVFDataset[String, Int]()
    for (item <- items) {
      dataset += makeDatum(item, gangs, featureLexicon, table, restricted, rescale, scaleRange)
    }

      dataset
  }

  def makeDatum(item:LexicalItem, gangs:Array[Gang], featureLexicon:Lexicon[String],
                table:HashMap[(String, String), Double], restricted:Boolean = false, rescale:Boolean = false,
                scaleRange:ScaleRange[Int]):RVFDatum[String, Int] = {

    val label = item.gangString

    // Make a Features Counter
    val counter = new Counter[Int]

    // For the DHPH similarities - find the overall similarity to all lexical items
    var overallSim:Double = 0.0
    for (i <- 0 until gangs.length) {
      for (m <- gangs(i).members) {
        val simToM = BPUtils.dhSimilarity(item, m, table)
        overallSim += simToM
      }
    }

    for (gangIndex <- 0 until gangs.length) {
      // Find the feature values for this gang
      val (min, max, avg) = findFeatures(item, gangs(gangIndex), table)
      val dhSim = (avg * gangs(gangIndex).size()) / overallSim

      // Find the feature indices for the features for this gang
      val minFeatureIndex = featureLexicon.get("g" + gangIndex + MIN_EXTENSION).getOrElse(-1)
      val maxFeatureIndex = featureLexicon.get("g" + gangIndex + MAX_EXTENSION).getOrElse(-1)
      val avgFeatureIndex = featureLexicon.get("g" + gangIndex + AVG_EXTENSION).getOrElse(-1)
      val dhSimFeatureIndex = featureLexicon.get("g" + gangIndex + DHSIM_EXTENSION).getOrElse(-1)
      val sizeFeatureIndex = featureLexicon.get("g" + gangIndex + SIZE_EXTENSION).getOrElse(-1)

      // Determine if you're resticted...
      var addIn:Boolean = true
      if (restricted) {
        // Find the singular form to restrict the candidate gangs
        val singularForm = gangs(gangIndex).getSingular()

        // If the singular form of the gang matches the singular form of the item, add the similarities
        if (singularForm != item.cvTemplateSgTrans) {
          addIn = false
        }
      }

      // Add the gang features to the Counter if not restricted
      if (addIn) {
        //counter.setCount(minFeatureIndex, min)
        counter.setCount(maxFeatureIndex, max)
        counter.setCount(avgFeatureIndex, avg)
        counter.setCount(sizeFeatureIndex, gangs(gangIndex).size())
        counter.setCount(dhSimFeatureIndex, dhSim)
      }


    }

    if (rescale) {
      val newCounter = Datasets.svmScaleDatum(counter, scaleRange, 0.0, 1.0)
      return new RVFDatum[String, Int](label, newCounter)
    }


    // Return an RVFDatum for this item
    new RVFDatum[String, Int](label, counter)
  }


  // Finds the min, max, and avg similarity of an item with a gang
  def findFeatures (item: LexicalItem, gang:Gang, table:HashMap[(String, String), Double]):(Double, Double, Double) = {
    // Calulate the max, min, and avg similarity between item and gang members
    var sumSimsForItem:Double = 0.0
    var maxSim:Double = Double.MinValue
    var minSim:Double = Double.MaxValue
    for (member <- gang.members) {
      val simToMember = BPUtils.dhSimilarity(item, member, table)
      sumSimsForItem += simToMember
      if (simToMember > maxSim) maxSim = simToMember
      if (simToMember < minSim) minSim = simToMember
    }
    val avgSim = sumSimsForItem / gang.members.length.toDouble

    (minSim, maxSim, avgSim)
  }


  def mkString ():String = "CluLab Processors 5.7.2 LogisticRegression"
}