package alife

import alife.util.Loops.*

/**
 * This class encapsulates the genome (a sequence of instructions), a label used in the visual highlighting code,
 * health and direction, the identifier, and some lifetime statistics of an individual.
 *
 * @param id the sequential identifier of the individual
 * @param genome the genome
 * @param myLabel the highlight-related label
 * @param myHealth the initial health of the individual
 * @param myDirection the initial direction of the individual
 */
class Individual(val id: Long, val genome: IArray[Instruction], private var myLabel: Int,
                 private var myHealth: Double, private var myDirection: Int):
  private var myLifeSpan: Int = 1
  private var myChildrenCount: Int = 0
  private var myTravelDistance: Int = 0
  private var myNecessaryInstructions: Int = -1
  private var pinnedToField: Boolean = false
  
  /**
   * Pins the individual to the field.
   * Will fail if the health is negative.
   * This locks changes to health and direction.
   */
  def pin(): Unit =
    checkAlive()
    pinnedToField = true
  
  /**
   * Unpins the individual from the field.
   * This allows changes to health and direction.
   */
  def unpin(): Unit = pinnedToField = false
  
  /**
   * Returns the weight of the individual, which is its genome length.
   * @return the weight of the individual.
   */
  def weight: Int = genome.length
  
  /**
   * Returns the current label of the individual.
   * @return the current label of the individual.
   */
  def label: Int = myLabel
  
  /**
   * Returns the current health of the individual.
   * @return the current health of the individual.
   */
  def health: Double = myHealth
  
  /**
   * Returns the current direction of the individual.
   * @return the current direction of the individual.
   */
  def direction: Int = myDirection

  /**
   * Sets a new label for the individual.
   * @param newLabel the new label value.
   */
  def setLabel(newLabel: Int): Unit =
    myLabel = newLabel
  
  /**
   * Sets a new health value for the individual.
   * This will fail if the individual is pinned to the field.
   * @param newHealth the new health value.
   */
  def setHealth(newHealth: Double): Unit =
    checkNotPinned()
    myHealth = newHealth
  
  /**
   * Sets a new direction for the individual.
   * This will fail if the individual is pinned to the field.
   * @param newDirection the new direction.
   */
  def setDirection(newDirection: Int): Unit =
    checkNotPinned()
    myDirection = newDirection
  
  /**
   * Creates a deep copy of this individual with all the internal fields. Used for checkpointing.
   * @return the deep copy of this individual.
   */
  def deepCopy(): Individual =
    val result = Individual(id, genome, myLabel, myHealth, myDirection)
    result.myLifeSpan = myLifeSpan
    result.myChildrenCount = myChildrenCount
    result.myTravelDistance = myTravelDistance
    result.myNecessaryInstructions = myNecessaryInstructions
    result.pinnedToField = pinnedToField
    result
  
  /**
   * Computes the number of necessary instructions for this genome, given the configuration.
   * An instruction is defined to be necessary if at least one of the action outputs depends on this instruction.
   * This number is cached in the individual because the config remains the same throughout the simulation.
   * @param config the life configuration to use. 
   * @return the number of necessary instructions.
   */
  def necessaryInstructions(config: Config): Int =
    if myNecessaryInstructions == -1 then
      val used = Array.ofDim[Boolean](genome.length)
      loopFromUntil(0, genome.length): ii =>
        val idx = genome.length - 1 - ii
        if ii < config.actions.length then used(idx) = true
        genome(idx).foreachReference: a =>
          if a > 0 && idx - a >= 0 then used(idx - a) = true
      val result = countFromUntil(0, used.length)(i => used(i))
      myNecessaryInstructions = result
    myNecessaryInstructions
  
  /**
   * Returns the lifespan of this individual.
   * This is formally defined as the number of simulation frames in which actions have been performed.
   * Every individual that is still alive participates in at least one simulation frame per simulation step,
   * but because of top-to-bottom left-to-right scanning, an individual may be lucky enough to get simulated more than
   * once per step.
   *
   * @return the lifespan.
   */
  def lifeSpan: Int = myLifeSpan
  
  /**
   * Returns the number of children directly produced by this individual. This does not include grandchildren.
   * @return the number of children of this individual.
   */
  def numberOfChildren: Int = myChildrenCount
  
  /**
   * Returns the travel distance of this individual.
   * This is formally defined as the number of `Move` actions performed during the lifespan.
   * Notes about how the lifespan is related to the simulation time also apply here, see `lifeSpan`.
   * @return the travel distance.
   */
  def travelDistance: Int = myTravelDistance
  
  /**
   * Returns the average speed of this individual.
   * This is formally defined as the number of `Move` actions divided by the number of all actions performed,
   * so is always at least 0 and at most 1.
   * This function returns 0 if the individual has not lived yet.
   * @return the average speed.
   */
  def averageSpeed: Double = if lifeSpan == 0 then 0 else travelDistance.toDouble / lifeSpan
  
  /**
   * This is used by the framework to record the action performed by this individual,
   * which updates the statistics.
   * @param action the action that was performed by this individual.
   */
  def recordAction(action: Action): Unit =
    myLifeSpan += 1
    action match
      case a: Action.Fork => myChildrenCount += 1
      case Action.Move => myTravelDistance += 1
      case _ =>

  private def checkNotPinned(): Unit =
    if pinnedToField then throw IllegalStateException("The individual is pinned")

  private def checkAlive(): Unit =
    if myHealth < 0 then throw IllegalStateException("The individual is dead")
