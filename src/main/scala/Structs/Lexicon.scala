package Structs

import scala.collection.mutable.HashMap

/**
 * Created by becky on 11/29/15.
 */
class Lexicon[A] () {
  val table = new HashMap[A, Int]

  def add (in:A): Int = {
    val v = table.getOrElse(in, -1)
    if (v != -1) return v
    else {
      val newIndex = table.keySet.size
      table(in) = newIndex
      return newIndex
    }
  }

  def getOrElse (in:A, default:Int) = table.getOrElse(in, default)

  def set(in:A, index:Int) {
    table(in) = index
  }

  def size() = table.size

}

class Counter[A] extends Lexicon[A] () {
  override def add (in:A): Int = {
    val v = table.getOrElse(in, -1)
    if (v != -1) {
      // Already there
      table(in) = v + 1
    }
    else {
      // new item
      table(in) = 1
    }

    table.get(in).get
  }

}
