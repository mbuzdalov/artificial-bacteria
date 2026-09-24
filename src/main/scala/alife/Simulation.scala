package alife

import alife.util.PseudoStack
import alife.util.Loops.*

import java.util.random.RandomGenerator.JumpableGenerator
import java.util.random.{RandomGenerator, RandomGeneratorFactory}
import scala.annotation.tailrec
import scala.compiletime.uninitialized

/**
 * This encapsulates the current state of the simulation and the simulation logic.
 *
 * @param config the configuration to use by this simulation run.
 * @param field the current state of the field in the simulation.
 * @param baseRandom the jumpable random number generator to seed frame-local random generators.
 */
class Simulation private (val config: Config, val field: Field, baseRandom: JumpableGenerator):
  private var currentFrameRandom: RandomGenerator = uninitialized
  private val callStack = PseudoStack() /* "volatile", do not copy */
  private val sineCache = Array.ofDim[Double](field.width) /* "volatile", do not copy */
  private var nIterationsPerformed = 0L
  private var nAliveBacteria = 0
  private var nBacteriaBornOverall = 0L
  private var nBacteriaDeadOverall = 0L
  
  initialize()
  
  /**
   * Performs a deep copy of the simulation state to create a checkpoint.
   * @return the deep copy of the simulation state.
   */
  def deepCopy(): Simulation =
    val result = new Simulation(config, field.deepCopy(), baseRandom.copy())
    result.nIterationsPerformed = nIterationsPerformed
    result.nAliveBacteria = nAliveBacteria
    result.nBacteriaBornOverall = nBacteriaBornOverall
    result.nBacteriaDeadOverall = nBacteriaDeadOverall
    result
  
  /**
   * Record the creation of a bacterium.
   * @param genome the genome of the bacterium.
   * @param label the label (used for UI coloring).
   * @param health the initial health.
   * @param direction the initial direction.
   * @param parent the parent of this bacterium (may be null).
   * @return the new bacterium.
   */
  def createBacterium(genome: IArray[Instruction], label: Int, health: Double, direction: Int, parent: Individual): Individual =
    nBacteriaBornOverall += 1
    Individual(nBacteriaBornOverall, genome, label, health, direction)
  
  /**
   * Record the death of a bacterium.
   * @param ind the bacterium that has just died.
   */
  def recordBacteriumDeath(ind: Individual): Unit =
    nBacteriaDeadOverall += 1
  
  /**
   * Returns the random number generator to use for all decisions related to the simulation.
   * Technical note: this generator is recreated before each frame starts, don't cache it.
   * @return the random number generator
   */
  def random: RandomGenerator = currentFrameRandom
  
  /**
   * Returns the number of iterations performed in this simulation run.
   * @return the number of iterations performed.
   */
  def iterations: Long = nIterationsPerformed
  
  /**
   * When called, modifies the food in the specified circle towards the given target amount of food.
   *
   * @param x the abscissa of the center of the circle.
   * @param y the ordinate of the center of the circle.
   * @param radius the radius of the circle.
   * @param targetAmount the target amount of food.
   */
  def increaseFood(x: Int, y: Int, radius: Int, targetAmount: Double): Unit =
    loopFromTo(-radius, radius): xi =>
      loopFromTo(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          val sq = field.getSquareChecked(x + xi, y + yi)
          val oldFood = sq.food
          sq.setFood(oldFood + (targetAmount - oldFood) * 0.5)
  
  /**
   * When called, erases everything (individuals, food, debris) in the specified circle.
   *
   * @param x the abscissa of the center of the circle.
   * @param y the ordinate of the center of the circle.
   * @param radius the radius of the circle.
   */
  def eraseEverything(x: Int, y: Int, radius: Int): Unit =
    loopFromTo(-radius, radius): xi =>
      loopFromTo(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          val sq = field.getSquareChecked(x + xi, y + yi)
          if sq.individual != null then recordBacteriumDeath(sq.individual)
          sq.eraseEverything()
  
  /**
   * Returns an individual closest to (`x`, `y`) at a distance not bigger than `maxDist`. If there are multiple such
   * individual, an arbitrary one is chosen.
   * @param x the abscissa of the center of the search area.
   * @param y the ordinate of the center of the search area.
   * @param maxDistance the maximum distance to consider before giving up.
   */
  def findAndDumpClosestIndividual(x: Int, y: Int, maxDistance: Int): Option[Individual] =
    @tailrec def impl(d: Int): Option[Individual] =
      if d > maxDistance then None else
        (-d to d).view.flatMap(dx => Seq(
          Option(field.getSquareChecked(x + dx, y + d - math.abs(dx)).individual),
          Option(field.getSquareChecked(x + dx, y - d + math.abs(dx)).individual),
        ).flatten).headOption match
          case Some(g) => Some(g)
          case None => impl(d + 1)
    impl(0)
  
  /**
   * This part of the simulation scans individuals in the top-to-bottom left-to-right order
   * and performs actions decided by the individuals found.
   * @return the array containing the numbers of actions performed for all configured actions.
   */
  private def performActionsOnIndividuals(): Array[Int] =
    val actions = config.actions
    val actionCount = Array.ofDim[Int](actions.size)
    
    loopFromUntil(0, field.height): y =>
      loopFromUntil(0, field.width): x =>
        val sq = field.getSquare(x, y)
        val ind = sq.individual
        if ind != null then
          val g = ind.genome
          callStack.clear()
          loopFromUntil(0, g.size): i =>
            callStack.push(g(i).apply(field, x, y, callStack))
          
          // Outputs are organized as follows:
          // action:            0             1                2      ...
          // output:  callStack(1)  callStack(2)     callStack(3)     ...
          // order:   most recent   2nd most recent  3rd most recent  ...
          
          var chosenAction = -1
          loopFromUntil(0, math.min(g.size, actions.size)): i =>
            if actions(i).canApply(this, x, y) then
              if chosenAction == -1 || callStack(i + 1) > callStack(chosenAction + 1) then
                chosenAction = i
          
          if chosenAction != -1 then
            val theAction = actions(chosenAction)
            ind.recordAction(theAction)
            theAction.apply(this, x, y)
            actionCount(chosenAction) += 1
    
    actionCount
  
  /**
   * This part of the simulation spends the idle energy for every individual found.
   */
  private def drainIdleEnergy(): Unit =
    loopFromUntil(0, field.height): y =>
      loopFromUntil(0, field.width): x =>
        val sq = field.getSquare(x, y)
        if sq.individual != null then
          val ind = sq.removeIndividual()
          val spentForLiving = math.min(config.idleCost, ind.health)
          sq.setDebris(sq.debris + spentForLiving * config.debrisFromActions)
          ind.setHealth(ind.health - config.idleCost)
          if ind.health >= 0
          then sq.setIndividual(ind)
          else recordBacteriumDeath(ind)
  
  /**
   * This part of the simulation deposits the food according to the configured logic
   * and performs configured debris conversions.
   */
  private def depositFoodAndConvertDebris(): Unit =
    val synthDecay = math.exp(-nIterationsPerformed * config.synthesisDecay) // initially 1, then decreases to 0
    val sineDecay = math.exp(-nIterationsPerformed * config.spotDecay) // initially 1, then decreases to 0
    
    // This is the average expected energy to deposit onto a square.
    // "Average" means it can go up and down, currently in a periodic way.
    // "Expected" means that the actual deposited amount is sampled u.a.r. from [0; the value determined for the square].
    val expectedFoodPerSquare = synthDecay * config.synthesisInit + (1 - synthDecay) * config.synthesisFinal
    
    val pi2 = 2 * math.Pi
    val spotXOffset = pi2 * nIterationsPerformed * config.spotSpeedX
    val spotYOffset = pi2 * nIterationsPerformed * config.spotSpeedY
    val spotXScale = pi2 * config.spotPeriodX / field.width
    val spotYScale = pi2 * config.spotPeriodY / field.height
    
    loopFromUntil(0, field.width): x =>
      sineCache(x) = (math.sin(x * spotXScale + spotXOffset) + 1) / 2

    val randomness = config.synthesisRandomness
    require(0 <= randomness && randomness <= 1, "synthesisRandomness should be in [0;1]")
    
    val foodPerSquareLowerBound = expectedFoodPerSquare * (1 - randomness)
    val foodPerSquareUpperBound = expectedFoodPerSquare * (1 + randomness)
    val debrisTotalDecay = math.max(0, 1 - config.debrisDegradation - config.debrisToFood)
    loopFromUntil(0, field.height): y =>
      val changeY = (math.sin(y * spotYScale + spotYOffset) + 1) / 2
      // This is what to multiply changeX by for newFoodScale.
      // See commends for the formula for newFoodScale below.
      val newFoodMultiple = (1 - sineDecay) * changeY * 4
      loopFromUntil(0, field.width): x =>
        val changeX = sineCache(x)
        val sq = field.getSquare(x, y)
        // What used to be here is:
        //   val newFoodScale = expectedFoodPerSquare * (sineDecay + (1 - sineDecay) * changeX * changeY * 4)
        // The product of changes is additionally multiplied by 4,
        // because the integral of changeX * changeY over the entire field is 1/4.
        // This way, `newFoodScale` is exactly `expectedFoodPerSquare` on average, which is what we want.
        //
        // Now, we compute the same thing,
        // but in a way that randomizes food per square using the synthesisRandomness field,
        // and with different parts computed at different times.
        val newFoodScale = sineDecay + newFoodMultiple * changeX
        val newFood = if randomness == 0.0
          then newFoodScale
          else newFoodScale * currentFrameRandom.nextDouble(foodPerSquareLowerBound, foodPerSquareUpperBound)
        val d2e = sq.debris * config.debrisToFood
        sq.setDebris(sq.debris * debrisTotalDecay)
        sq.setFood(sq.food + d2e + newFood)
  
  /**
   * This computes all statistics defined in `StepStatistics` for the current state of the field,
   * also given the array of action counts produced by `performActionsOnIndividuals`.
   * @param actionCount the array of action counts to populate statistics.
   * @return the statistics for the current step.
   */
  private def computeStatistics(actionCount: Array[Int]): StepStatistics =
    var maxHealth = 0.0
    var sumHealths = 0.0
    var totalFood = 0.0
    var maxFood = 0.0
    var maxGenomeSize = 0
    var sumGenomeSizes = 0L
    var nMonsters = 0
    
    var maxLifeSpan = 0
    var maxChildren = 0
    var maxDistance = 0
    var maxSpeed = 0.0
    
    var sumNecessaryInstructions = 0
    var sumNecessaryInstructionRates = 0.0

    nAliveBacteria = 0
    
    loopFromUntil(0, field.height): y =>
      loopFromUntil(0, field.width): x =>
        val cell = field.getSquare(x, y)
        totalFood += cell.food
        maxFood = math.max(maxFood, cell.food)
        val ind = cell.individual
        if ind != null then
          nAliveBacteria += 1
          sumHealths += ind.health
          maxHealth = math.max(maxHealth, ind.health)
          if ind.label < 0 then nMonsters += 1
          maxGenomeSize = math.max(maxGenomeSize, ind.genome.size)
          sumGenomeSizes += ind.genome.size
          maxLifeSpan = math.max(maxLifeSpan, ind.lifeSpan)
          maxChildren = math.max(maxChildren, ind.numberOfChildren)
          maxDistance = math.max(maxDistance, ind.travelDistance)
          maxSpeed = math.max(maxSpeed, ind.averageSpeed)
          sumNecessaryInstructions += ind.necessaryInstructions(config)
          sumNecessaryInstructionRates += ind.necessaryInstructions(config).toDouble / ind.genome.size
    
    assert(nBacteriaBornOverall - nBacteriaDeadOverall == nAliveBacteria)
    
    StepStatistics(
      iteration = nIterationsPerformed,
      maxGenomeSize = maxGenomeSize,
      averageGenomeSize = sumGenomeSizes.toDouble / nAliveBacteria,
      numberOfBacteria = nAliveBacteria,
      averageHealth = sumHealths / nAliveBacteria,
      maximalHealth = maxHealth,
      totalFood = totalFood,
      maxFood = maxFood,
      actionCounts = IArray.unsafeFromArray(actionCount),
      maxLife = maxLifeSpan,
      maxChildren = maxChildren,
      maxTravelDistance = maxDistance,
      maxSpeed = maxSpeed,
      nMonsters = nMonsters,
      avgNecessaryInstructions = sumNecessaryInstructions.toDouble / nAliveBacteria,
      avgNecessaryInstructionRatio = sumNecessaryInstructionRates / nAliveBacteria,
    )
  
  /**
   * Initializes the field randomly as configured.
   */
  private def initialize(): Unit =
    currentFrameRandom = baseRandom.copyAndJump()
    loopFromUntil(0, field.height): y =>
      loopFromUntil(0, field.width): x =>
        val cell = field.getSquare(x, y)
        cell.eraseEverything()
        cell.setFood(1e-9)
        if currentFrameRandom.nextDouble() < config.initialBacteriaProbability then
          val genome = IArray.tabulate(config.initialGenomeLength)(i => Instruction.random(this, i))
          val individual = createBacterium(genome, 0, config.initialHealth, currentFrameRandom.nextInt(4), null)
          cell.setIndividual(individual)
          nAliveBacteria += 1
  
  /**
   * Performs a single simulation step and returns the statistics computed after performing the step.
   * @return the statistics for the step just performed.
   */
  def simulationStep(): StepStatistics =
    currentFrameRandom = baseRandom.copyAndJump()
    nIterationsPerformed += 1
    val actionCount = performActionsOnIndividuals()
    drainIdleEnergy()
    depositFoodAndConvertDebris()
    computeStatistics(actionCount)
  
  /**
   * Returns whether simulation can continue.
   * @return whether simulation can continue.
   */
  def canContinue: Boolean = nAliveBacteria > 0

object Simulation:
  /**
   * This creates a new simulation given the (prototype) configuration and creates all other resources on its own.
   * The prototype configuration is different from the actual configuration to be used in that
   * the random seed may be set to 0, in which case the new (time-based) seed will be generated.
   *
   * @param protoConfig the prototype configuration to use.
   */
  def apply(protoConfig: Config): Simulation =
    val config = protoConfig.withFixedSeed
    val result = new Simulation(
      config = config,
      field = Field(config.fieldWidth, config.fieldHeight),
      baseRandom = RandomGeneratorFactory.of[JumpableGenerator](config.randomFactory).create(config.randomSeed)
    )

    println(s"Runtime context created with random factory '${config.randomFactory}' and ${
      if protoConfig.randomSeed == 0
      then s"NEW time-based seed ${config.randomSeed}"
      else s"fixed seed ${config.randomSeed}"
    }")
    
    result
