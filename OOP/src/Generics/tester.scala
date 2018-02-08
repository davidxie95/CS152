package Generics

object tester {
  def main(args: Array[String]): Unit = {
    val waitingList = genericQueue[String]

    waitingList.enqueue("One")
    waitingList.enqueue("Two")
    waitingList.enqueue("Three")
    waitingList.enqueue("Four")
    waitingList.enqueue("Five")
    
    println(waitingList.toString)

    while (waitingList.isEmpty == false) {
      waitingList.dequeue
      println(waitingList.toString)
    }
  }
}