package brokenPlurals

import scala.collection.mutable

/**
 * Created by becky on 6/2/17.
 */
object EvaluationUtils {


  /**
   * Normalized discounted cumulative gain, equation from Manning book p. 163
   *
   *   NDCG(Q, k) = 1/|Q| ( ∑_{j=1:|Q|} ( Zkj  ∑_{m=1:k} ((2^R(j,m)^ − 1) / (log_2(1 + m)))
   *
   * @param gains
   * @return
   */
  def normalizedDCG(gains: Array[Array[Double]], numGolds: Seq[Int], maxGain: Double): Double = {
    if (gains.isEmpty) return 0.0
    val normalized = for {
      (instance, instanceIndex) <- gains.zipWithIndex
      instanceNormalizedDCG = Z_kj(instance.length, numGolds(instanceIndex), maxGain) * manningDCG(instance)
    } yield instanceNormalizedDCG
    // Return the average
    normalized.sum / normalized.length.toDouble
  }

  def Z_kj(instanceLength: Int, numGold: Int, maxGain: Double): Double = {
    val idealRanking = Array.fill[Double](numGold)(maxGain) ++ Array.fill[Double](instanceLength - numGold)(0.0)
    1.0 / manningDCG(idealRanking)
  }

  def manningDCG(gains: Seq[Double]): Double = {
    val discountedGains = for {
      (gain, index) <- gains.zipWithIndex
      discountedGain = (math.pow(2.0, gain) - 1.0) / log2(index.toDouble + 2.0)  // The formula adds one, here I add another bc of 0-indexing
    } yield discountedGain
    discountedGains.sum
  }

  // Helper method for above, computes the log base 2
  def log2(x: Double): Double = math.log10(x)/math.log10(2.0)


  def calculateGains(surveyItems: Array[SurveyItem], rankings: Array[Array[(String, Double)]]): Array[Array[Double]] = {
    for {
      (surveyItem, index) <- surveyItems.zipWithIndex
      ranking = rankings(index)
      rankedGains = ranking.map(gangAndScore => gainForClassification(gangAndScore._1, surveyItem))
    } yield rankedGains
  }

  def gainForClassification(gangString: String, surveyItem: SurveyItem): Double = {
    if (surveyItem.gangStrings.contains(gangString)) 1.0 else 0.0
  }

  /**
   * Return a normalized discounted cumulative gain, as well as the top n gangs
   * @param surveyItems -- the SurveyItems from the csv file
   * @param rankings -- for each SurveyItem, the ranked (gangString, Score) from the classifiers
   */
  def evaluateRankingNDCG(surveyItems: Array[SurveyItem], rankings: Array[Array[(String, Double)]]): (Double, Array[Double]) = {
    val gains = calculateGains(surveyItems, rankings)
    // DEBUG DISPLAY--------
//    println("First SurveyItem:")
//    val correct = surveyItems.head.gangStrings.mkString(", ")
//    println(s"SG TRANS: ${surveyItems.head.sgTrans}")
//    println(s"  Correct gangs: $correct")
//    val r = rankings.head
//    for (i <- r.indices) {
//      println (s"$i - GangAssigned=${r(i)._1}  Score=${r(i)._2}  GAIN=${gains.head(i)}  (gold $correct)")
//    }
//    //rankings.head.foreach(r => println("\t" + r))
//    sys.exit(0)
    // ---------------------
    val numGolds = surveyItems.map(_.gangStrings.size)
    val surveyItemGains = calculateGains(surveyItems, rankings)
    val indexedInstances = surveyItemGains.zipWithIndex
    val ndgsForStats = indexedInstances.map(
      instance => Z_kj(instance._1.length, numGolds(instance._2), 1.0) * manningDCG(instance._1))

    (normalizedDCG(gains, numGolds, maxGain = 1.0), ndgsForStats)
  }

}
