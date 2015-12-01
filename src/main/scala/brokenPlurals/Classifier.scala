package brokenPlurals

import Structs.Counter

import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
 * Created by becky on 11/29/15.
 */
trait Classifier {
  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double], k:Int):Array[(String, Int)]

  def mkString ():String
}


object DHPH2014_GCM extends Classifier {

  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double], k:Int = 0):Array[(String, Int)] = {
    val classifications = new Array[(String,Int)](items.length)

    for (iIdx <- 0 until items.length){

      val item = items(iIdx)

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

  def mkString():String = "Dawdy-Hesterberg & Pierrehumbert (2014) -- GCM"
}

object DHPH2014_restrictedGCM extends Classifier {

  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double], k:Int = 0):Array[(String, Int)] = {
    val classifications = new Array[(String,Int)](items.length)
    val numItems = items.length

    // Keep track of the number of gangs chosen between for post analysis
    var minNumCandGangs:Int = 10000
    var maxNumCandGangs:Int = -1
    var avgNumCandGangs:Double = -1.0

    for (iIdx <- 0 until numItems){

      val item = items(iIdx)

      // Initialize variables for the fold
      var numCandGangs:Int = 0                      // Keeps Track of the eligible gangs for this item
      val similarityToGangs = Array.fill[Double](gangs.length)(0.0)
      var allSimilarities:Double = 0.0

      // Find the similarity to each gang
      for (i <- 0 until gangs.length) {
        val gang = gangs(i)

        // Find the singular for to restrict the candidate gangs
        val singularForm = gang.gangString.split("-")(0).split("").slice(2,1000).mkString("")

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
  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double], k:Int):Array[(String, Int)] = {
    val classifications = new Array[(String,Int)](items.length)
    // TODO: implement - change the method to be knn
    for (iIdx <- 0 until items.length){

      val item = items(iIdx)

      // Initialize variables
      val similarities = new ArrayBuffer[(Double, Int)]

//      val similarityToGangs = Array.fill[Double](gangs.length)(0.0)
//      var allSimilarities:Double = 0.0

      // Find the similarity to each gang
      for (i <- 0 until gangs.length) {
        val gang = gangs(i)

        // Find and store the similarity to the gang member
        for (member <- gang.members) {
          val simToMember = BPUtils.dhSimilarity(item, member, table)
          similarities.append((simToMember, i))

//          similarityToGangs(i) += simToMember
//          allSimilarities += simToMember
        }
      }
      // Store the similarities to all the gangs for this item
      //    These are the S_{i,C_j} values for each j in K (K is the set of all gangs) from DH-PH 2014
      //val finalSimilarities = similarityToGangs.map(x => x / allSimilarities)

      // Find the best gang, handling ties
//      var maxSim:Double = Double.MinValue
//      var bestGang = new ArrayBuffer[Int]
//      for (gIdx <- 0 until gangs.length) {
//        if (finalSimilarities(gIdx) > maxSim) {
//          maxSim = finalSimilarities(gIdx)
//          bestGang = new ArrayBuffer[Int]
//          bestGang.append(gIdx)
//        } else if (finalSimilarities(gIdx) == maxSim) {
//          bestGang.append(gIdx)
//        }
//      }

      // Sort the neighbors
      val sortedNeighbors = similarities.sortBy(- _._1)

      // Retrive the top k
      val topK = sortedNeighbors.slice(0, k)

      // Find the most common
      val gangCounter = new Counter[Int]
      for (j <- 0 until k) gangCounter.add(topK(j)._2)

      // Check that some gangs occur more than once
      var maxGangs:Int = 1
      var modeGang:Int = -1
      for (k <- gangCounter.table.keys) {
        val count = gangCounter.getOrElse(k, -1)
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

//      // Store the classification as well as number of items tied for "best"
//      classifications(iIdx) = (gangs(bestGang.head).gangString, bestGang.length)
    }


    classifications
  }

  def mkString():String = "KNN"
}