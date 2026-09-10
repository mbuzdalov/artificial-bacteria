package alife

import alife.util.Loops
import alife.Instruction.*

/**
 * This class encapsulates the genome (a sequence of instructions), a label used in the visual highlighting code,
 * and some lifetime statistics of an individual.
 *
 * @param genome the genome
 * @param label the highlight-related label
 */
case class Individual(genome: IArray[Instruction], label: Int):
  private var myLifeSpan: Int = 1
  private var myChildren: Int = 0
  private var myTravelDistance: Int = 0
  private var myNecessaryInstructions: Int = -1
  
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
      Loops.foreach(0, genome.length): ii =>
        val idx = genome.length - 1 - ii
        if ii < config.actions.length then used(idx) = true
        Instruction.forEachArgument(genome(idx)): a =>
          if a > 0 && idx - a >= 0 then used(idx - a) = true
      val result = Loops.count(0, used.length)(i => used(i))
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
  def numberOfChildren: Int = myChildren
  
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
      case a: Action.Fork => myChildren += 1
      case Action.Move => myTravelDistance += 1
      case _ =>
