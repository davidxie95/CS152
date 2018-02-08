package DnD

class Dragon(val name: String, var health: Int = 100) extends Character(name) {
  def attack(victim: Knight, damage: Int) = {
    if (health > 0 && victim.health > 0) {
      println(name + " is flaming " + victim.name)
      victim.health -= damage
      if (victim.health < 0) {
        victim.health = 0
      }
      println(victim.name + "'s health: " + victim.health)
    }
  }
}
object Dragon {
  def apply(name: String) = { new Dragon(name) }

}