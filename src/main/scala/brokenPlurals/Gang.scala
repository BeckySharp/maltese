package brokenPlurals

import scala.collection.mutable.ArrayBuffer

/**
 * Created by becky on 11/28/15.
 */
class Gang (var gangString:String) {
  val members = new ArrayBuffer[LexicalItem]

  def add (in:LexicalItem) = members.append(in)

  def getSingular():String = gangString.split("-")(0).split("").slice(2,1000).mkString("")

  def size():Int = members.length

}
