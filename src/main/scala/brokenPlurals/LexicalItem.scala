package brokenPlurals

import edu.arizona.sista.struct.Lexicon

/**
 * Created by becky on 11/28/15.
 */
class LexicalItem (val sgOrth:String, val plOrth:String, val sgTrans:String, val plTrans:String, val gender:String,
                   val gloss:String, val paperType:Int, val cvTemplatePlTrans:String ) {

  var gang:Int = -1
  var gangString:String = ""
  var cvTemplateSgTrans:String = ""
  var cvTemplateSgOrth:String = ""
  var cvTemplatePlOrth:String = ""

}

class SurveyItem (val sgTrans: String, val plTrans: Set[String], val vowels:Array[String]) {
  var gangs:Set[Int] = Set[Int]()
  val cvTemplateSgTrans:String = BPUtils.makeCVTemplate(sgTrans, vowels)
  val cvTemplatesPlTrans:Set[String] = convertPlTransToTemplates(plTrans, vowels)
  var gangStrings:Set[String] = cvTemplatesPlTrans.map(pluralTemplate => s"[$cvTemplateSgTrans-$pluralTemplate]")

  private def convertPlTransToTemplates(plurals: Set[String], vowels: Array[String]): Set[String] = {
    plurals.map(p => BPUtils.makeCVTemplate(p, vowels))
  }

  def fillInGangIndices(gangLexicon: Lexicon[String]) = {
    gangs = gangStrings.map(gangLexicon.add)
  }

  def toLexicalItem(): LexicalItem = new LexicalItem("", "", sgTrans, "", "", "", 0, "")
}
