package alife

import java.util.concurrent.ThreadLocalRandom

/**
 * Evolutionary operators.
 */
object Operators {
  def mutate(genome: Seq[Instruction]): Seq[Instruction] = {
    val rng = ThreadLocalRandom.current()
    rng.nextInt(3) match {
      case 0 =>
        genome.zipWithIndex.map(t => if (rng.nextInt(genome.size) == 0) Instruction.random(t._2) else t._1)
      case 1 =>
        if (genome.nonEmpty) {
          val (h, t) = genome.splitAt(rng.nextInt(genome.size))
          h ++ t.tail
        } else genome
      case 2 =>
        val (h, t) = genome.splitAt(rng.nextInt(1 + genome.size))
        (h :+ Instruction.random(h.size)) ++ t
      case _ => throw new AssertionError()
    }
  }
}
