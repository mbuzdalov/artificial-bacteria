package alife

/**
 * A trait for all actions.
 */
sealed trait Action:
  /**
   * Tests whether this action can be applied for the bacterium at the given coordinates of the given field
   * using the provided simulation constants.
   * @param field the field.
   * @param x the X coordinate.
   * @param y the Y coordinate.
   * @param config the configuration that defines how simulation works.
   * @return `true` if the action can be performed, `false` otherwise.
   */
  def canApply(field: Field, x: Int, y: Int, config: Config): Boolean
  
  /**
   * Applies the action to the bacterium at the given coordinates of the given field
   * using the provided simulation constants.
   * @param field the field.
   * @param x the X coordinate.
   * @param y the Y coordinate.
   * @param config the configuration that defines how simulation works.
   */
  def apply(field: Field, x: Int, y: Int, config: Config): Unit

/**
 * All the available actions.
 */
object Action:
  /**
   * Rotates the individual by the amount of ticks clockwise given as `rotation`.
   * @param rotation the amount of rotation ticks to perform.
   */
  abstract class Rotate(rotation: Int) extends Action:
    override def canApply(field: Field, x: Int, y: Int, config: Config): Boolean = true
    override def apply(field: Field, x: Int, y: Int, config: Config): Unit =
      val cell = field.getCell(x, y)
      val e = config.rotationCost * (cell.health + cell.weight)
      cell.setIndividual(cell.individual, (cell.direction + rotation) & 3, cell.health - e)
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
    override def canApply(field: Field, x: Int, y: Int, config: Config): Boolean =
      field.getRelativeCell(x, y, Field.relativeLocationForward).health == 0
    override def apply(field: Field, x: Int, y: Int, config: Config): Unit =
      assert(canApply(field, x, y, config))
      val cell = field.getCell(x, y)
      val e = config.moveCost * (cell.health + cell.weight + cell.debris)
      val g = cell.individual
      val h = cell.health
      val d = cell.direction
      field.getRelativeCell(x, y, Field.relativeLocationForward).setIndividual(g, d, h - e)
      cell.setIndividual(null, 0, 0)
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
    override def canApply(field: Field, x: Int, y: Int, config: Config): Boolean = true
    override def apply(field: Field, x: Int, y: Int, config: Config): Unit =
      val cell = field.getCell(x, y)
      val w = cell.weight
      // how much can we eat before hitting our global limit
      val intakeLimitGlobal = w * config.healthMultiple - cell.health
      // how much can we eat technically: min of current food and of the max increment
      val intakeLimitLocal = math.min(w * config.healthIncrementMultiple, cell.food)
      val toEat = math.max(0, math.min(intakeLimitGlobal, intakeLimitLocal))
      cell.setFood(cell.food - toEat)
      cell.setIndividual(cell.individual, cell.direction, cell.health + toEat * (1 - config.eatCost))
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
    override def canApply(field: Field, x: Int, y: Int, config: Config): Boolean =
      Move.canApply(field, x, y, config)
    override def apply(field: Field, x: Int, y: Int, config: Config): Unit =
      assert(canApply(field, x, y, config))
      val cell = field.getCell(x, y)
      val e = config.forkCost * cell.weight
      val g = cell.individual
      val h = cell.health - e
      val d = cell.direction
      if h / 2 > 0 then
        cell.setIndividual(mutation.mutate(g, config.random), d, h / 2)
        Move.apply(field, x, y, config)
      cell.setIndividual(g, d, h / 2)
      cell.setDebris(cell.debris + e * config.debrisFromActions)
