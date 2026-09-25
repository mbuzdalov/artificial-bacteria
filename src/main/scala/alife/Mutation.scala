package alife

import alife.util.Loops.loopFromUntil

/**
 * Trait for all mutation operators.
 */
trait Mutation:
  /**
   * Creates a mutated copy of the given individual. A new individual will always be returned.
   *
   * @param individual the individual to mutate.
   * @param sim the simulation to be used.
   * @return the mutated individual.
   */
  def mutate(individual: Individual, sim: Simulation): Individual

/**
 * Known implementations for mutation operators.
 */
object Mutation:
  private def standardMutation(genome: IArray[Instruction], sim: Simulation): IArray[Instruction] =
    val genomeCopy = genome.unsafeArray.clone()
    val rng = sim.random
    loopFromUntil(0, genomeCopy.length): i =>
      if rng.nextInt(genome.size) == 0 then genomeCopy(i) = sim.randomInstruction(i)
    IArray.unsafeFromArray(genomeCopy)
  
  private def cutInstruction(genome: IArray[Instruction], index: Int): Array[Instruction] =
    val ga = genome.unsafeArray
    val result = Array.ofDim[Instruction](ga.length - 1)
    System.arraycopy(ga, 0, result, 0, index)
    System.arraycopy(ga, index + 1, result, index, result.length - index)
    result
  
  private def insertInstruction(genome: IArray[Instruction], index: Int, sim: Simulation): Array[Instruction] =
    val ga = genome.unsafeArray
    val result = Array.ofDim[Instruction](ga.length + 1)
    System.arraycopy(ga, 0, result, 0, index)
    result(index) = sim.randomInstruction(index)
    System.arraycopy(ga, index, result, index + 1, ga.length - index)
    result
  
  /**
   * The original "primitive" mutation operator. There are three mutation options:
   * 0) with probability `1 - applicationProbability`, no change is applied.
   * 1) each instruction is replaced with a random one with probability 1 / genome size;
   * 2) a random instruction gets deleted (if the genome is empty, this does nothing);
   * 3) a random instruction is inserted at a random position (including before-first and after-last).
   */
  case class Primitive(applicationProbability: Double) extends Mutation:
    require(0 < applicationProbability && applicationProbability <= 1.0, "Probability should be in (0;1]")
    override def mutate(individual: Individual, sim: Simulation): Individual =
      val genome = individual.genome
      val rng = sim.random
      val newGenome = if rng.nextDouble() >= applicationProbability then genome else rng.nextInt(3) match
        case 0 => standardMutation(genome, sim)
        case 1 => if genome.isEmpty then genome else IArray.unsafeFromArray(cutInstruction(genome, rng.nextInt(genome.length)))
        case 2 => IArray.unsafeFromArray(insertInstruction(genome, rng.nextInt(genome.length + 1), sim))
      sim.createBacterium(newGenome, individual.label, individual.health, individual.direction, individual)
  
  /**
   * The "smooth" mutation operator. There are three mutation options:
   * 0) with probability `1 - applicationProbability`, no change is applied.
   * 1) each instruction is replaced with a random one with probability 1 / genome size;
   * 2) a random instruction gets deleted (if the genome is empty, this does nothing) and the instructions following the deletion get references fixed if possible.
   * 3) a random instruction is inserted at a random position (including before-first and after-last) and the instructions following the insertion get references fixed.
   */
  case class Smooth(applicationProbability: Double) extends Mutation:
    require(0 < applicationProbability && applicationProbability <= 1.0, "Probability should be in (0;1]")
    override def mutate(individual: Individual, sim: Simulation): Individual =
      val rng = sim.random
      val genome = individual.genome
      val newGenome = if rng.nextDouble() >= applicationProbability then genome else rng.nextInt(3) match
        case 0 => standardMutation(genome, sim)
        case 1 => if genome.isEmpty then genome else
          val index = rng.nextInt(genome.length)
          val temp = cutInstruction(genome, index)
          // indices `i` and below should stay (`index` == `i` points to element following the deletion)
          // indices above `i + 1` need a -1, index `i + 1` was deleted (-1 is safe)
          loopFromUntil(index, temp.length): i =>
            temp(i) = temp(i).mapReferences(a => if a > i - index then a - 1 else a)
          IArray.unsafeFromArray(temp)
        case 2 =>
          val index = rng.nextInt(genome.length + 1)
          val temp = insertInstruction(genome, index, sim)
          // indices `i` and below should stay (`index` == `i` points to element following the insertion)
          // indices above `i + 1` need a +1
          loopFromUntil(index + 1, temp.length): i =>
            temp(i) = temp(i).mapReferences(a => if a > i - index - 1 then a + 1 else a)
          IArray.unsafeFromArray(temp)
        case _ => throw AssertionError()
      sim.createBacterium(newGenome, individual.label, individual.health, individual.direction, individual)
  
  /**
   * The mutation operator which does not mutate at all.
   */
  case object NoChange extends Mutation:
    override def mutate(individual: Individual, sim: Simulation): Individual =
      sim.createBacterium(individual.genome, individual.label, individual.health, individual.direction, individual)
