package value

import collection.mutable._
import context._
import expression._

class Store(private var elems: ArrayBuffer[Value] = ArrayBuffer[Value]()) extends Value {

  // adds e to the end of store
  def add(e: Value) { elems += e }

  // inserts e at position pos in this
  def put(e: Value, pos: Integer) { elems.insert(pos.value, e) }

  // removes element at position pos from this
  def rem(pos: Integer) { elems.remove(pos.value) }

  // returns element at position pos in this
  def get(pos: Integer): Value = elems(pos.value)

  // returns true ie this contains e
  def contains(e: Value): Boole = if (elems.contains(e)) Boole(true) else Boole(false)

  // returns the size of this
  def size: Integer = Integer(elems.size)

  // returns "{e0 e1 e2 ...}"
  override def toString = { "{" + elems.mkString(" ") + "}" }

  // returns store containing the elements of this transformed by trans
  // currently hard coded
  def map(trans: Closure): Store = {

    var temp = ArrayBuffer[Value]()
    for (x <- elems) {
      val x = Integer(99)
      temp += x
    }
    //trans.apply(temp.toList)
    new Store(ArrayBuffer(temp: _*))
  }

  // returns store containing the elements of this that passed test
  def filter(test: Closure): Store = {

    val list = elems.toList.filter { x => x.isInstanceOf[Integer] }
    new Store(ArrayBuffer(list: _*))
  }
}