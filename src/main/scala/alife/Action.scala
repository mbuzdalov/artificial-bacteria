package alife

/**
 * A trait for all actions.
 */
sealed trait Action:
  /**
   * Tests whether this action can be applied for the bacterium for the given simulation.
   * @param sim the simulation.
   * @param x the X coordinate.
   * @param y the Y coordinate.
   * @return `true` if the action can be performed, `false` otherwise.
   */
  def canApply(sim: Simulation, x: Int, y: Int): Boolean
  
  /**
   * Applies the action to the bacterium at the given coordinates for the given simulation.
   * @param sim the simulation.
   * @param x the X coordinate.
   * @param y the Y coordinate.
   */
  def apply(sim: Simulation, x: Int, y: Int): Unit

/**
 * All the available actions.
 */
object Action:
  /**
   * Rotates the individual by the amount of ticks clockwise given as `rotation`.
   * @param rotation the amount of rotation ticks to perform.
   */
  abstract class Rotate(rotation: Int) extends Action:
    override def canApply(sim: Simulation, x: Int, y: Int): Boolean = true
    override def apply(sim: Simulation, x: Int, y: Int): Unit =
      val cell = sim.field.getCell(x, y)
      val ind = cell.removeIndividual()
      val config = sim.config
      val e = config.rotationCost * (ind.health + ind.weight)
      ind.setDirection((ind.direction + rotation) & 3)
      ind.setHealth(ind.health - e)
      if ind.health >= 0 
        then cell.setIndividual(ind)
        else sim.recordBacteriumDeath(ind)
      cell.setDebris(cell.debris + e * config.debrisFromActions)
  
  /**
   * The negative (counter-clockwise) rotation:
   * 1) This action can always be applied.
   * 2) The emergy required for this action is (bacterium's health + bacterium's weight) times `rotationCost`.
   * 3) This energy times `debrisFromActions` is deposited as debris.
   */
  case object RotateMinus extends Rotate(-1)
  
  /**
   * The positive (clockwise) rotation:
   * 1) This action can always be applied.
   * 2) The energy required for this action is (bacterium's health + bacterium's weight) times `rotationCost`.
   * 3) This energy times `debrisFromActions` is deposited as debris.
   */
  case object RotatePlus extends Rotate(+1)
  
  /**
   * The move one step forward in the frontal direction of the bacterium.:
   * 1) This action can only be applied if the destination cell contains no other bacteria.
   * 2) The energy required for this action is (bacterium's health + bacterium's weight + debris in current cell) times `moveCost`.
   * 3) This energy times `debrisFromActions` is deposited as debris.
   */
  case object Move extends Action:
    override def canApply(sim: Simulation, x: Int, y: Int): Boolean =
      sim.field.getRelativeCell(x, y, Field.relativeLocationForward).individual == null
    override def apply(sim: Simulation, x: Int, y: Int): Unit =
      assert(canApply(sim, x, y))
      val field = sim.field
      val config = sim.config
      val cell = field.getCell(x, y)
      val nextCell = field.getRelativeCell(x, y, Field.relativeLocationForward)
      val ind = cell.removeIndividual()
      val e = config.moveCost * (ind.health + ind.weight + cell.debris)
      ind.setHealth(ind.health - e)
      if ind.health >= 0
        then nextCell.setIndividual(ind)
        else sim.recordBacteriumDeath(ind)
      cell.setDebris(cell.debris + e * config.debrisFromActions)
 
  /**
   * The food consumption action:
   * 1) This action can always be applied.
   * 2) The amount of the food eaten is the minimum of:
   *    - the slack of the bacterium's health to the global limit, which is bacterium's weight times `healthMultiple`
   *    - the single intake amount, which is bacterium's weight times `healthIncrementMultiple`
   *    - the amount of food in the current cell
   * 3) Not all food can be eaten, (1 - `eatCost`) is spent as an energy required for assisting the consumption.
   * 4) This energy times `debrisFromActions` is deposited as debris.
   */
  case object Eat extends Action:
    override def canApply(sim: Simulation, x: Int, y: Int): Boolean = true
    override def apply(sim: Simulation, x: Int, y: Int): Unit =
      val cell = sim.field.getCell(x, y)
      val ind = cell.removeIndividual()
      val config = sim.config
      val w = ind.weight
      // how much can we eat before hitting our global limit
      val intakeLimitGlobal = w * config.healthMultiple - ind.health
      // how much can we eat technically: min of current food and of the max increment
      val intakeLimitLocal = math.min(w * config.healthIncrementMultiple, cell.food)
      val toEat = math.max(0, math.min(intakeLimitGlobal, intakeLimitLocal))
      cell.setFood(cell.food - toEat)
      ind.setHealth(ind.health + toEat * (1 - config.eatCost))
      if ind.health >= 0 
        then cell.setIndividual(ind)
        else sim.recordBacteriumDeath(ind)
      cell.setDebris(cell.debris + toEat * config.eatCost * config.debrisFromActions)
  
  /**
   * The fork action, which creates another bacterium that is a mutant of the current one:
   * 1) This action can only be applied if the cell in the frontal direction contains no bacteria.
   * 2) The energy required for this action is bacterium's weight times `forkCost`.
   * 3) This energy times `debrisFromActions` is deposited as debris.
   * 4) The health is divided between the old and the new bacteria.
   * 5) The new bacteria additionally incurs all move costs (from the current to the target cell).
   * 
   * @param mutation the mutation operator to apply
   */
  case class Fork(mutation: Mutation) extends Action:
    override def canApply(sim: Simulation, x: Int, y: Int): Boolean =
      Move.canApply(sim, x, y)
    override def apply(sim: Simulation, x: Int, y: Int): Unit =
      assert(canApply(sim, x, y))
      val cell = sim.field.getCell(x, y)
      val ind = cell.removeIndividual()
      val config = sim.config
      val e = config.forkCost * ind.weight
      val h = ind.health - e
      ind.setHealth(h / 2)
      if ind.health >= 0 then
        val mutant = mutation.mutate(ind, sim)
        cell.setIndividual(mutant)
        Move.apply(sim, x, y)
        cell.setIndividual(ind)
      else sim.recordBacteriumDeath(ind)  
      cell.setDebris(cell.debris + e * config.debrisFromActions)
