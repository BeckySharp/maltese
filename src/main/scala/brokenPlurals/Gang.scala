package brokenPlurals

import scala.collection.mutable.ArrayBuffer

/**
 * Created by becky on 11/28/15.
 */
class Gang {
  val members = new ArrayBuffer[LexicalItem]

  def add (in:LexicalItem) = members.append(in)

  def size():Int = members.length

}
