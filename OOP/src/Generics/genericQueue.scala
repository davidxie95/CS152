package Generics

import scala.collection.mutable.ArrayBuffer

class genericQueue[T](queue: ArrayBuffer[T] = new ArrayBuffer[T]) {

  //add
  def enqueue(i: T) = {
    queue += i
  }
  //delete
  def dequeue = queue.remove(0)

  def isEmpty = queue.isEmpty

  override def toString = {
    queue.toString()
  }
}

object genericQueue {
  def apply[T] = {
    new genericQueue[T]
  }
}
