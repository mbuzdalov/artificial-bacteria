package alife

case class Individual(genome: Seq[Instruction], label: Int) {
  private[this] var myLifeSpan: Int = 1
  private[this] var myChildren: Int = 0
  private[this] var myTravelDistance: Int = 0

  def lifeSpan: Int = myLifeSpan
  def numberOfChildren: Int = myChildren
  def travelDistance: Int = myTravelDistance
  def averageSpeed: Double = travelDistance.toDouble / lifeSpan

  def recordAction(action: Action): Unit = {
    myLifeSpan += 1
    action match {
      case Action.Fork => myChildren += 1
      case Action.Move => myTravelDistance += 1
      case _ =>
    }
  }
}
