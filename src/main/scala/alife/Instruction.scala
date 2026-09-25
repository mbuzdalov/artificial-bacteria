package alife

import alife.util.PseudoStack

import java.util.random.RandomGenerator

/**
 * A trait for all Cartesian Genetic Programming instructions used in this system.
 * Every instruction returns a `Double` value
 * by (optionally) querying the field and the results of preceding instructions.
 */
sealed trait Instruction:
  /**
   * Computes the value of the instruction using the information provided.
   * The `data` argument deserves an explanation. `data(i)` refers to the result of computation of the `i`-th instruction
   * before the current one. That is, `data(1)` returns the result of the previous instruction executed,
   * `data(2)` the one preceding that, and so on. Illegal values result in 0 being returned,
   * which also includes `data(0)` being currently computed.
   *
   * @param field the field to query.
   * @param x the X coordinate of the current individual.
   * @param y the Y coordinate of the current individual.
   * @param data the accessor to the previous computation results.
   * @return the computed value.
   */
  def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double
  
  /**
   * Creates a new instruction of the same type with each reference argument replaced using the given function.
   * @param mapper the function that converts reference arguments.
   * @return the new converted instruction.
   */
  def mapReferences(mapper: Int => Int): Instruction
  
  /**
   * Applies the given function to each of the reference arguments.
   * @param body the function that is applied to reference arguments.
   */
  def foreachReference(body: Int => Unit): Unit

/**
 * All the available instructions.
 */
object Instruction:
  trait RandomFactory:
    def generate(position: Int, random: RandomGenerator): Instruction

  /**
   * Always returns a constant value.
   * @param value the constant value.
   */
  final class Const(value: Double) extends Instruction:
    override def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = value
    override def foreachReference(body: Int => Unit): Unit = ()
    override def mapReferences(mapper: Int => Int): Instruction = this

  object Const extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction = Const(random.nextDouble())
  
  /**
   * Returns the weight of the current individual.
   */
  object MyWeight extends Instruction, RandomFactory:
    override def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getSquare(x, y).individual.weight
    override def foreachReference(body: Int => Unit): Unit = ()
    override def mapReferences(mapper: Int => Int): Instruction = this
    override def generate(position: Int, random: RandomGenerator): Instruction = this
  
  /**
   * Returns the health of the current individual.
   */
  object MyHealth extends Instruction, RandomFactory:
    override def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getSquare(x, y).individual.health
    override def foreachReference(body: Int => Unit): Unit = ()
    override def mapReferences(mapper: Int => Int): Instruction = this
    override def generate(position: Int, random: RandomGenerator): Instruction = this
  
  /**
   * Returns the food amount at a given relative location to the current individual.
   * @param relativeLocation the relative location.
   */
  class FoodAt(relativeLocation: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getRelativeSquare(x, y, relativeLocation).food
    override def foreachReference(body: Int => Unit): Unit = ()
    override def mapReferences(mapper: Int => Int): Instruction = this
  
  object FoodAt extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      FoodAt(random.nextInt(Field.numberOfRelativeLocations))
  
  /**
   * Returns the amount of debris at a given relative location to the current individual.
   * @param relativeLocation the relative location.
   */
  class DebrisAt(relativeLocation: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      field.getRelativeSquare(x, y, relativeLocation).debris
    override def foreachReference(body: Int => Unit): Unit = ()
    override def mapReferences(mapper: Int => Int): Instruction = this
  
  object DebrisAt extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      DebrisAt(random.nextInt(Field.numberOfRelativeLocations))
  
  /**
   * Returns the health of an individual at a given relative location to the current individual, 0 if none.
   * @param relativeLocation the relative location.
   */
  class HealthAt(relativeLocation: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      val other = field.getRelativeSquare(x, y, relativeLocation).individual
      if other != null then other.health else 0.0
    override def foreachReference(body: Int => Unit): Unit = ()
    override def mapReferences(mapper: Int => Int): Instruction = this
  
  object HealthAt extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      HealthAt(random.nextInt(Field.numberOfRelativeLocations))
  
  /**
   * Returns the sine of the other instruction's value.
   * @param arg the index of the other instruction acting as an argument.
   */
  class Sin(arg: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = math.sin(data(arg))
    override def foreachReference(body: Int => Unit): Unit = body(arg)
    override def mapReferences(mapper: Int => Int): Instruction = Sin(mapper(arg))
  
  object Sin extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Sin(random.nextInt(position + 1))
  
  /**
   * Returns the cosine of the other instruction's value.
   * @param arg the index of the other instruction acting as an argument.
   */
  class Cos(arg: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = math.cos(data(arg))
    override def foreachReference(body: Int => Unit): Unit = body(arg)
    override def mapReferences(mapper: Int => Int): Instruction = Cos(mapper(arg))
    
  object Cos extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Cos(random.nextInt(position + 1))
  
  /**
   * Returns the exponent of the other instruction's value.
   * @param arg the index of the other instruction acting as an argument.
   */
  class Exp(arg: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = math.exp(data(arg))
    override def foreachReference(body: Int => Unit): Unit = body(arg)
    override def mapReferences(mapper: Int => Int): Instruction = Exp(mapper(arg))
  
  object Exp extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Exp(random.nextInt(position + 1))
  
  /**
   * Returns the logarithm of the other instruction's value.
   * @param arg the index of the other instruction acting as an argument.
   */
  class Log(arg: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = math.log(data(arg))
    override def foreachReference(body: Int => Unit): Unit = body(arg)
    override def mapReferences(mapper: Int => Int): Instruction = Log(mapper(arg))
  
  object Log extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Log(random.nextInt(position + 1))
  
  /**
   * Returns the sum of values of two other instructions.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  class Plus(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = data(arg1) + data(arg2)
    override def foreachReference(body: Int => Unit): Unit = { body(arg1); body(arg2) }
    override def mapReferences(mapper: Int => Int): Instruction = Plus(mapper(arg1), mapper(arg2))

  object Plus extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Plus(random.nextInt(position + 1), random.nextInt(position + 1))
  
  /**
   * Returns the difference of values of two other instructions.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  class Minus(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = data(arg1) - data(arg2)
    override def foreachReference(body: Int => Unit): Unit = { body(arg1); body(arg2) }
    override def mapReferences(mapper: Int => Int): Instruction = Minus(mapper(arg1), mapper(arg2))

  object Minus extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Minus(random.nextInt(position + 1), random.nextInt(position + 1))
    
  /**
   * Returns the product of values of two other instructions.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  class Times(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = data(arg1) * data(arg2)
    override def foreachReference(body: Int => Unit): Unit = { body(arg1); body(arg2) }
    override def mapReferences(mapper: Int => Int): Instruction = Times(mapper(arg1), mapper(arg2))
  
  object Times extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Times(random.nextInt(position + 1), random.nextInt(position + 1))
  
  /**
   * Returns the ratio of values of two other instructions.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  class Divide(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double = data(arg1) / data(arg2)
    override def foreachReference(body: Int => Unit): Unit = { body(arg1); body(arg2) }
    override def mapReferences(mapper: Int => Int): Instruction = Divide(mapper(arg1), mapper(arg2))

  object Divide extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Divide(random.nextInt(position + 1), random.nextInt(position + 1))
  
  /**
   * Returns the soft comparison of values of two other instructions:
   * 1) if the arguments are equal, 0 is returned;
   * 2) if the first argument is smaller than the second one, a positive value is returned: the bigger the difference, the closer the value to +1;
   * 3) if the first argument is larger than the first one, a negative value is returned: the bigger the difference, the closer the value to -1.
   * @param arg1 the index of the other instruction serving as the first argument.
   * @param arg2 the index of the other instruction serving as the second argument.
   */
  class Sigmoid(arg1: Int, arg2: Int) extends Instruction:
    override final def apply(field: Field, x: Int, y: Int, data: PseudoStack): Double =
      2 / (1 + math.exp(data(arg1) - data(arg2))) - 1
    override def foreachReference(body: Int => Unit): Unit = { body(arg1); body(arg2) }
    override def mapReferences(mapper: Int => Int): Instruction = Sigmoid(mapper(arg1), mapper(arg2))
  
  object Sigmoid extends RandomFactory:
    override def generate(position: Int, random: RandomGenerator): Instruction =
      Sigmoid(random.nextInt(position + 1), random.nextInt(position + 1))
  
  /**
   * Generates a new random instruction for the given position in the individual.
   * @param sim the current simulation.
   * @param position the 0-based position of the instruction being generated.
   * @return the random instruction.
   */
  def random(sim: Simulation, position: Int): Instruction =
    val random = sim.random
    def nextPos() = random.nextInt(position + 1)
    def nextLoc() = random.nextInt(Field.numberOfRelativeLocations)

    random.nextInt(15) match
      case 0 => Const(random.nextDouble())
      case 1 => MyWeight
      case 2 => MyHealth
      case 3 => FoodAt(nextLoc())
      case 4 => DebrisAt(nextLoc())
      case 5 => HealthAt(nextLoc())
      case 6 => Sin(nextPos())
      case 7 => Cos(nextPos())
      case 8 => Exp(nextPos())
      case 9 => Log(nextPos())
      case 10 => Plus(nextPos(), nextPos())
      case 11 => Minus(nextPos(), nextPos())
      case 12 => Times(nextPos(), nextPos())
      case 13 => Divide(nextPos(), nextPos())
      case 14 => Sigmoid(nextPos(), nextPos())
      case _ => throw new AssertionError()
