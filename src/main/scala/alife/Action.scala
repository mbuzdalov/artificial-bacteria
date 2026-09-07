package alife

import alife.Field.Constants

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
   * @param constants the constants that define how simulation works.
   * @return `true` if the action can be performed, `false` otherwise.
   */
  def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean
  
  /**
   * Applies the action to the bacterium at the given coordinates of the given field
   * using the provided simulation constants.
   * @param field the field.
   * @param x the X coordinate.
   * @param y the Y coordinate.
   * @param constants the constants that define how simulation works.
   */
  def apply(field: Field, x: Int, y: Int, constants: Constants): Unit
  
  /**
   * Returns the 0-based index of this action in the `Action.all(...)` array for performance reasons.
   * In other words, `Action.all(this.index) == this`.
   * @return the index.
   */
  def index: Int

/**
 * All the available actions.
 */
object Action:
  /**
   * All supported actions, pre-created and indexable.
   */
  final val all: IArray[Action] = IArray(Fork, RotateMinus, RotatePlus, Move, Eat)
  all.indices.foreach(i => assert(all(i).index == i))
  
  /**
   * Rotates the individual by the amount of ticks clockwise given as `rotation`.
   * @param rotation the amount of rotation ticks to perform.
   * @param index the index of the action.
   */
  abstract class Rotate(rotation: Int, val index: Int) extends Action:
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean = true
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit =
      assert(canApply(field, x, y, constants))
      val e = constants.rotationCost * (field.getHealth(x, y) + field.getWeight(x, y))
      field.setIndividual(x, y, field.getIndividual(x, y), (field.getDirection(x, y) + rotation) & 3, field.getHealth(x, y) - e)
      field.setDebris(x, y, field.getDebris(x, y) + e * constants.debrisFromActions)
  
  /**
   * The negative (counter-clockwise) rotation:
   * 1) This action can always be applied.
   * 2) The emergy required for this action is (bacterium's health + bacterium's weight) times `rotationCost`.
   * 3) This energy times `debrisFromActions` is deposited as debris.
   */
  case object RotateMinus extends Rotate(-1, 1)
  
  /**
   * The positive (clockwise) rotation:
   * 1) This action can always be applied.
   * 2) The energy required for this action is (bacterium's health + bacterium's weight) times `rotationCost`.
   * 3) This energy times `debrisFromActions` is deposited as debris.
   */
  case object RotatePlus  extends Rotate(+1, 2)
  
  /**
   * The move one step forward in the frontal direction of the bacterium.:
   * 1) This action can only be applied if the destination cell contains no other bacteria.
   * 2) The energy required for this action is (bacterium's health + bacterium's weight + debris in current cell) times `moveCost`.
   * 3) This energy times `debrisFromActions` is deposited as debris.
   */
  case object Move extends Action:
    override def index: Int = 3
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean =
      field.getHealthRelative(x, y, Field.relativeLocationForward) == 0
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit =
      assert(canApply(field, x, y, constants))
      val e = constants.moveCost * (field.getHealth(x, y) + field.getWeight(x, y) + field.getDebris(x, y))
      val g = field.getIndividual(x, y)
      val h = field.getHealth(x, y)
      val d = field.getDirection(x, y)
      field.setIndividualRelative(x, y, Field.relativeLocationForward, g, d, h - e)
      field.setIndividual(x, y, null, 0, 0)
      field.setDebris(x, y, field.getDebris(x, y) + e * constants.debrisFromActions)
  
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
    override def index: Int = 4
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean = true
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit =
      assert(canApply(field, x, y, constants))
      val w = field.getWeight(x, y)
      // how much can we eat before hitting our global limit
      val intakeLimitGlobal = w * constants.healthMultiple - field.getHealth(x, y)
      // how much can we eat technically: min of current food and of the max increment
      val intakeLimitLocal = math.min(w * constants.healthIncrementMultiple, field.getEnergy(x, y))
      val eatAmount = math.max(0, math.min(intakeLimitGlobal, intakeLimitLocal))
      field.setEnergy(x, y, field.getEnergy(x, y) - eatAmount)
      field.setIndividual(x, y, field.getIndividual(x, y), field.getDirection(x, y), field.getHealth(x, y) + eatAmount * (1 - constants.eatCost))
      field.setDebris(x, y, field.getDebris(x, y) + eatAmount * constants.eatCost * constants.debrisFromActions)
  
  /**
   * The fork action, which creates another bacterium that is a mutant of the current one:
   * 1) This action can only be applied if the cell in the frontal direction contains no bacteria.
   * 2) The energy required for this action is bacterium's weight times `forkCost`.
   * 3) This energy times `debrisFromActions` is deposited as debris.
   * 4) The health is divided between the old and the new bacteria.
   * 5) The new bacteria additionally incurs all move costs (from the current to the target cell).
   */
  case object Fork extends Action:
    override def index: Int = 0
    override def canApply(field: Field, x: Int, y: Int, constants: Constants): Boolean =
      field.getHealthRelative(x, y, Field.relativeLocationForward) == 0
    override def apply(field: Field, x: Int, y: Int, constants: Constants): Unit =
      assert(canApply(field, x, y, constants))
      assert(Move.canApply(field, x, y, constants))
      val e = constants.forkCost * field.getWeight(x, y)
      val g = field.getIndividual(x, y)
      val h = field.getHealth(x, y) - e
      val d = field.getDirection(x, y)
      if h / 2 > 0 then
        field.setIndividual(x, y, Operators.mutate(g), d, h / 2)
        Move.apply(field, x, y, constants)
      field.setIndividual(x, y, g, d, h / 2)
      field.setDebris(x, y, field.getDebris(x, y) + e * constants.debrisFromActions)
