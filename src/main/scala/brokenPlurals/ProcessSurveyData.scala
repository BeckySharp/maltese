package brokenPlurals

/**
 * Created by becky on 6/2/17.
 */
object ProcessSurveyData {

  def main(args: Array[String]): Unit = {

    // Load the Singular-Plural pairs
    val lexicalItems = BrokenPlurals.loadCSV("/home/becky/Downloads/broken_plural.csv")

    // Load the Similarity table
    val similarityTableFile = "/home/becky/Documents/maltesePhonemeFeatures.stb"
    val similarityTable = BPUtils.loadSimilarities(similarityTableFile)

    // Make a set of the vowels (for generating CV templates of other forms)
    val vowelSet = BrokenPlurals.makeVowelSet(lexicalItems)

    // Generate CVTemplates for the singular forms
    BPUtils.generateSingularTemplates(lexicalItems, vowelSet)

    // Assign Gangs to each item
    val (gangs, gangLexicon, gangCounter) = BrokenPlurals.makeGangs(lexicalItems)
    val (filteredGangs, filteredLexicon) = BrokenPlurals.filterGangs(gangs, gangCounter, threshold = 4)

    // Load the survey items
    val surveyItems = BrokenPlurals.loadSurveyDataCSV("/home/becky/Downloads/BrokenPluralsResponses052017_trans.csv", vowelSet)

    // Fill in the gang indices for the survey items
    surveyItems.foreach(_.fillInGangIndices(filteredLexicon))

    // Make temp LexicalItems from the SurveyItems
    val surveyLexicalItems = surveyItems.map(_.toLexicalItem)
    // Generate CVTemplates for the singular forms
    BPUtils.generateSingularTemplates(surveyLexicalItems, vowelSet)


    // split the data into folds
    val numFolds:Int = 5
    val numTrials:Int = 1
    //val testOn = "DEV"
    val testOn = "TEST"
    //val trialFolds = makeFolds(filteredGangs, filteredLexicon, numFolds, numTrials)
    val trialFolds = BrokenPlurals.makeClassicFolds(filteredGangs, filteredLexicon, numFolds, numTrials)

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

    val (ndcgsForAllFolds1, avgNDCG1, instanceNDCGsForStats1) = BrokenPlurals.doCrossValidationSurveyRanking(
      classifierMethod1,
      trialFolds(0),
      similarityTable,
      k = 5,
      testOn,
      restricted1,
      filteredLexicon,
      surveyItems,
      surveyLexicalItems
    )

    // Display:
    println ("\n=================================================================================================================")
    println (s"\tclassifier 1: ${classifierMethod1.mkString()}\n\tnumFolds: $numFolds")
    println ("=================================================================================================================")
    println ("                                        Final Results - testing on: " + testOn)
    println ("=================================================================================================================\n")
    println ("NDCGs across all folds: " + ndcgsForAllFolds1.mkString("\t"))
    println ("")
    println ("Final (Averaged) NDCG: " + avgNDCG1)

    // ----------------------------------------------------------------------------------------------------
    //  Classification Method 2
    // ----------------------------------------------------------------------------------------------------

    //val classifierMethod2 = DHPH2014_GCM
    val classifierMethod2 = DHPH2014_restrictedGCM
    //val classifierMethod2 = kNearestNeighbors
    //val classifierMethod2 = LogisticRegression
    //    val restricted1:Boolean = false
    val restricted2:Boolean = false

    //sys.exit(0)

    val (ndcgsForAllFolds2, avgNDCG2, instanceNDCGsForStats2) = BrokenPlurals.doCrossValidationSurveyRanking(
      classifierMethod2,
      trialFolds(0),
      similarityTable,
      k = 5,
      testOn,
      restricted1,
      filteredLexicon,
      surveyItems,
      surveyLexicalItems
    )

    // Display:
    println ("\n=================================================================================================================")
    println (s"\tclassifier 2: ${classifierMethod2.mkString()}\n\tnumFolds: $numFolds")
    println ("=================================================================================================================")
    println ("                                        Final Results - testing on: " + testOn)
    println ("=================================================================================================================\n")
    println ("NDCGs across all folds: " + ndcgsForAllFolds2.mkString("\t"))
    println ("")
    println ("Final (Averaged) NDCG: " + avgNDCG2)

    // Statistical Analysis
    val nSamples = 10000
    // the first should be the baseline
    val pValue = BrokenPlurals.runStats(instanceNDCGsForStats2, instanceNDCGsForStats1, nSamples, randomSeed = 6)
    // Display:
    println ("\n\n=================================================================================================================")
    println (s"\tp-value: $pValue \t (using $nSamples samples)")
    println ("=================================================================================================================")



  }

  // todo: merge with accuracies
  // todo: spit out top k rankings per item
  // todo: precision@1 for surveyItems from diff methods
  // todo: redo stats for accuracies

}
