package DnD

object Dungeon {
  val random = new scala.util.Random(System.nanoTime())
  def main(args: Array[String]): Unit = {
    val puff = new Dragon("Puff")
    val thor = new Knight("Thor")
    while (puff.health > 0 && thor.health > 0) {
      thor.attack(puff, random.nextInt(25))
      puff.attack(thor, random.nextInt(25))
    }
  }

}

