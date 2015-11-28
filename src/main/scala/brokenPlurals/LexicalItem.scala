package brokenPlurals

/**
 * Created by becky on 11/28/15.
 */
class LexicalItem (val sgOrth:String, val plOrth:String, val sgTrans:String, val plTrans:String, val gender:String,
                   val gloss:String, val paperType:Int, val cvTemplatePlTrans:String ) {

  var gang:Int = -1
  var cvTemplateSgTrans:String = ""
  var cvTemplateSgOrth:String = ""
  var cvTemplatePlOrth:String = ""

}
