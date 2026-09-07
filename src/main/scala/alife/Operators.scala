package alife

import java.util.concurrent.ThreadLocalRandom

/**
 * Evolutionary operators.
 */
object Operators:
  /**
   * The simple mutation operator. There are three mutation options:
   * 1) each instruction is replaced with a random one with probability 1 / genome size;
   * 2) a random instruction gets deleted (if the genome is empty, this does nothing);
   * 3) a random instruction is inserted at a random position (including before-first and after-last).
   *
   * This always creates a new individual, even if the genome is unchanged, so that the stats are reset.
   *
   * @param individual the individual to mutate
   * @return the new individual
   */
  def mutate(individual: Individual): Individual =
    val rng = ThreadLocalRandom.current()
    val genome = individual.genome
    val newGenome = rng.nextInt(3) match
      case 0 =>
        genome.zipWithIndex.map(t => if rng.nextInt(genome.size) == 0 then Instruction.random(t._2) else t._1)
      case 1 =>
        if genome.isEmpty then genome else
          val (h, t) = genome.splitAt(rng.nextInt(genome.size))
          h ++ t.tail
      case 2 =>
        val (h, t) = genome.splitAt(rng.nextInt(1 + genome.size))
        (h :+ Instruction.random(h.size)) ++ t
      case _ => throw new AssertionError()
    individual.copy(genome = newGenome)
