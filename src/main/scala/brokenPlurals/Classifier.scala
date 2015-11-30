package brokenPlurals

import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
 * Created by becky on 11/29/15.
 */
trait Classifier {
  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double]):Array[(String, Int)]
}


object DHPH2014_GCM extends Classifier {

  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double]):Array[(String, Int)] = {
    val classifications = new Array[(String,Int)](items.length)

    for (iIdx <- 0 until items.length){

      val item = items(iIdx)

      val similarityToGangs = Array.fill[Double](gangs.size)(0.0)
      var allSimilarities:Double = 0.0

      // Find the similarity to each gang
      for (i <- 0 until gangs.size) {
        val gang = gangs(i)

        // Find and store the similarity to the gang member
        for (member <- gang.members) {
          val simToMember = BPUtils.dhSimilarity(item, member, table)
          similarityToGangs(i) += simToMember
          allSimilarities += simToMember
        }
      }

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
}

object DHPH2014_restrictedGCM extends Classifier {

  def classify (gangs:Array[Gang], items:Array[LexicalItem], table: HashMap[(String, String), Double]):Array[(String, Int)] = {
    val classifications = new Array[(String,Int)](items.length)
    val numItems = items.length

    // Keep track of the number of gangs chosen between for post analysis
    var minNumCandGangs:Int = 10000
    var maxNumCandGangs:Int = -1
    var avgNumCandGangs:Double = -1.0

    for (iIdx <- 0 until numItems){

      val item = items(iIdx)

      // Keep Track of the eligible gangs for this item
      var numCandGangs:Int = 0

      val similarityToGangs = Array.fill[Double](gangs.length)(0.0)
      var allSimilarities:Double = 0.0

      // Find the similarity to each gang
      for (i <- 0 until gangs.length) {
        val gang = gangs(i)

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

      // Update the cand gang trackers
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
}