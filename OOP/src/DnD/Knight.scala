package DnD

class Knight(val name: String, var health: Int = 100) extends Character(name) {
  def attack(victim: Dragon, damage: Int) = {
    if (health > 0 && victim.health > 0) {
      println(name + " is stabbing " + victim.name)
      victim.health -= damage
      if (victim.health < 0) {
        victim.health = 0
      }
      println(victim.name + "'s health: " + victim.health)
    }
  }
}

object Knight {
  def apply(name: String) = { new Knight(name) }

}