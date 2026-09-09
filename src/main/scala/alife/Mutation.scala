package alife

import java.util.random.RandomGenerator

/**
 * Trait for all mutation operators.
 */
trait Mutation:
  /**
   * Creates a mutated copy of the given individual. A new individual will always be returned.
   *
   * @param individual the individual to mutate.
   * @param rng        the random generator to be used.
   * @return the mutated individual.
   */
  def mutate(individual: Individual, rng: RandomGenerator): Individual

/**
 * Known implementations for mutation operators.
 */
object Mutation:
  /**
   * The original "primitive" mutation operator. There are three mutation options:
   * 1) each instruction is replaced with a random one with probability 1 / genome size;
   * 2) a random instruction gets deleted (if the genome is empty, this does nothing);
   * 3) a random instruction is inserted at a random position (including before-first and after-last).
   */
  case object Primitive extends Mutation:
    override def mutate(individual: Individual, rng: RandomGenerator): Individual =
      val genome = individual.genome
      val newGenome = rng.nextInt(3) match
        case 0 =>
          genome.zipWithIndex.map: (v, i) =>
            if rng.nextInt(genome.size) == 0 then Instruction.random(rng, i) else v
        case 1 =>
          if genome.isEmpty then genome else
            val (h, t) = genome.splitAt(rng.nextInt(genome.size))
            h ++ t.tail
        case 2 =>
          val (h, t) = genome.splitAt(rng.nextInt(1 + genome.size))
          (h :+ Instruction.random(rng, h.size)) ++ t
        case _ => throw new AssertionError()
      individual.copy(genome = newGenome)
  
  /**
   * The "smooth" mutation operator. There are three mutation options:
   * 1) each instruction is replaced with a random one with probability 1 / genome size;
   * 2) a random instruction gets deleted (if the genome is empty, this does nothing) and the instructions following the deletion get references fixed if possible.
   * 3) a random instruction is inserted at a random position (including before-first and after-last) and the instructions following the insertion get references fixed.
   */
  case object Smooth extends Mutation:
    override def mutate(individual: Individual, rng: RandomGenerator): Individual =
      val genome = individual.genome
      val newGenome = rng.nextInt(3) match
        case 0 =>
          genome.zipWithIndex.map: (v, i) =>
            if rng.nextInt(genome.size) == 0 then Instruction.random(rng, i) else v
        case 1 =>
          if genome.isEmpty then genome else
            val (h, t) = genome.splitAt(rng.nextInt(genome.size))
            val newTail = t.tail.zipWithIndex.map: (v, i) =>
              // indices `i` and below should stay (`index` == `i` points to element following the deletion)
              // indices above `i + 1` need a -1, index `i + 1` was deleted (-1 is safe)
              Instruction.mapArguments(a => if a > i then a - 1 else a)(v)
            h ++ newTail
        case 2 =>
          val (h, t) = genome.splitAt(rng.nextInt(1 + genome.size))
          val newTail = t.zipWithIndex.map: (v, i) =>
            // indices `i` and below should stay (`index` == `i` points to element following the insertion)
            // indices above `i + 1` need a +1
            Instruction.mapArguments(a => if a > i then a + 1 else a)(v)
          (h :+ Instruction.random(rng, h.size)) ++ newTail
        case _ => throw new AssertionError()
      individual.copy(genome = newGenome)
