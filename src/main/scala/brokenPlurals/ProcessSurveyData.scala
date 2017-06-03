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

    val classifierMethod1 = DHPH2014_GCM
    //val classifierMethod1 = DHPH2014_restrictedGCM
    //val classifierMethod1 = kNearestNeighbors
    //val classifierMethod1 = LogisticRegression
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

  }

}
