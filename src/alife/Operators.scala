package alife

import java.util.concurrent.ThreadLocalRandom

/**
 * Evolutionary operators.
 */
object Operators {
  def mutate(individual: Individual): Individual = {
    val rng = ThreadLocalRandom.current()
    val genome = individual.genome
    rng.nextInt(3) match {
      case 0 =>
        individual.copy(genome = genome.zipWithIndex.map(t => if (rng.nextInt(genome.size) == 0) Instruction.random(t._2) else t._1))
      case 1 =>
        if (genome.nonEmpty) {
          val (h, t) = genome.splitAt(rng.nextInt(genome.size))
          individual.copy(genome = h ++ t.tail)
        } else individual
      case 2 =>
        val (h, t) = genome.splitAt(rng.nextInt(1 + genome.size))
        individual.copy(genome = (h :+ Instruction.random(h.size)) ++ t)
      case _ => throw new AssertionError()
    }
  }
}
