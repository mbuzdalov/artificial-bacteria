package alife

import alife.persistence.{Persistor, PersistorFactory}
import alife.util.PseudoStack
import alife.util.Loops.*

import java.util.random.RandomGenerator.JumpableGenerator
import java.util.random.{RandomGenerator, RandomGeneratorFactory}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

/**
 * This encapsulates the current state of the simulation and the simulation logic.
 *
 * @param config the configuration to use by this simulation run.
 * @param field the current state of the field in the simulation.
 * @param baseRandom the jumpable random number generator to seed frame-local random generators.
 * @param persistor the (nullable) persistence engine used to cache action computation of individuals
 */
class Simulation private (val config: Config, val field: Field, baseRandom: JumpableGenerator, persistor: Persistor)
extends AutoCloseable:
  if persistor != null then
    require(config == persistor.config, "Configurations in the arguments do not match")
  
  private var currentFrameRandom: RandomGenerator = uninitialized
  private val callStack = PseudoStack() /* "volatile", do not copy */
  private val sineCache = Array.ofDim[Double](field.width) /* "volatile", do not copy */
  private val genealogyListeners = ArrayBuffer[Simulation.GenealogyListener]()
  private var nIterationsPerformed = 0L
  private var nAliveBacteria = 0
  private var nBacteriaBornOverall = 0L
  private var nBacteriaDeadOverall = 0L
  private var isClosed = false
  private var isOutsideIterationProper = true
  
  initialize()
  
  /**
   * Adds the specified listener to watch for genealogy events.
   * Adding the same listener will result in receiving same messages multiple times.
   * @param listener the listener.
   */
  def addGenealogyListener(listener: Simulation.GenealogyListener): Unit =
    genealogyListeners.addOne(listener)
  
  /**
   * Removes the specified listener to (no longer) watch for genealogy events.
   * This removes only the first copy of a listener, comparing by `equals`.
   * If there is no such listener, nothing changes.
   * @param listener the listener.
   */
  def removeGenealogyListener(listener: Simulation.GenealogyListener): Unit =
    val index = genealogyListeners.indexOf(listener)
    if index >= 0 then genealogyListeners.remove(index)
  
  /**
   * Performs a deep copy of the simulation state to create a checkpoint.
   * @return the deep copy of the simulation state.
   */
  def deepCopy(): Simulation =
    checkNotClosed()
    checkOutsideIteration()
    val result = new Simulation(config, field.deepCopy(), baseRandom.copy(), persistor)
    result.nIterationsPerformed = nIterationsPerformed
    result.nAliveBacteria = nAliveBacteria
    result.nBacteriaBornOverall = nBacteriaBornOverall
    result.nBacteriaDeadOverall = nBacteriaDeadOverall
    result.isClosed = isClosed
    result.isOutsideIterationProper = isOutsideIterationProper
    result
  
  private def createBacteriumImpl(genome: IArray[Instruction], label: Int, health: Double, direction: Int, parent: Individual): Individual =
    nBacteriaBornOverall += 1
    val result = Individual(nBacteriaBornOverall, genome, label, health, direction)
    genealogyListeners.foreach(_.bacteriumBorn(result, parent, nIterationsPerformed))
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
    checkNotClosed()
    checkInsideIteration()
    createBacteriumImpl(genome, label, health, direction, parent)
  
  private def recordBacteriumDeathImpl(ind: Individual): Unit =
    genealogyListeners.foreach(_.bacteriumDead(ind, nIterationsPerformed))
    nBacteriaDeadOverall += 1
  
  /**
   * Record the death of a bacterium.
   * @param ind the bacterium that has just died.
   */
  def recordBacteriumDeath(ind: Individual): Unit =
    checkNotClosed()
    checkInsideIteration()
    recordBacteriumDeathImpl(ind)
  
  /**
   * Returns the random number generator to use for all decisions related to the simulation.
   * Technical note: this generator is recreated before each frame starts, don't cache it.
   * @return the random number generator
   */
  def random: RandomGenerator =
    checkNotClosed()
    currentFrameRandom
  
  /**
   * Returns the number of iterations performed in this simulation run.
   * @return the number of iterations performed.
   */
  def iterations: Long =
    checkNotClosed()
    nIterationsPerformed
  
  /**
   * Generates a random instruction for the given position, as specified in the config.
   * @param position the position to generate a random instruction for.
   * @return the generated instruction.
   */
  def randomInstruction(position: Int): Instruction =
    config.randomInstructionFactory(random).generate(position, random)
  
  /**
   * When called, puts a monster with the specified genome to the specified cell of the field.
   * The existing individual, if any, is killed and removed.
   * @param x the abscissa where to put the monster.
   * @param y the ordinate where to put the monster.
   * @param genome the genome of the monster.
   */
  def placeMonster(x: Int, y: Int, genome: IArray[Instruction]): Unit =
    checkNotClosed()
    checkCanPerformInteractions()
    val sq = field.getSquare(x, y)
    val previousIndividual = sq.individual
    if previousIndividual != null then
      sq.removeIndividual()
      recordBacteriumDeathImpl(previousIndividual)
    val monster = createBacteriumImpl(genome, -1, config.initialHealth, random.nextInt(4), null)
    sq.setIndividual(monster)
  
  /**
   * When called, modifies the food in the specified circle towards the given target amount of food.
   *
   * @param x the abscissa of the center of the circle.
   * @param y the ordinate of the center of the circle.
   * @param radius the radius of the circle.
   * @param targetAmount the target amount of food.
   */
  def increaseFood(x: Int, y: Int, radius: Int, targetAmount: Double): Unit =
    checkNotClosed()
    checkCanPerformInteractions()
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
    checkNotClosed()
    checkCanPerformInteractions()
    loopFromTo(-radius, radius): xi =>
      loopFromTo(-radius, radius): yi =>
        if xi * xi + yi * yi <= radius * radius then
          val sq = field.getSquareChecked(x + xi, y + yi)
          if sq.individual != null then recordBacteriumDeathImpl(sq.individual)
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
    checkNotClosed()
    impl(0)
  
  /**
   * Returns whether one can perform interactive actions (interactions) within the current iteration.
   * @return whether one can perform interactions.
   */
  def canPerformInteractions: Boolean = persistor == null
  
  private def checkCanPerformInteractions(): Unit =
    if !canPerformInteractions then throw IllegalStateException("Cannot perform interactions")
  
  private def checkInsideIteration(): Unit =
    if isOutsideIterationProper
    then throw IllegalStateException("A modification that shall happen only inside an iteration was called outside of an iteration")
  
  private def checkOutsideIteration(): Unit =
    if !isOutsideIterationProper
    then throw IllegalStateException("State copying cannot happen when inside an iteration")
  
  private def checkNotClosed(): Unit =
    if isClosed then throw IllegalStateException("close() has been called on this Simulation")
  
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
          val action = if persistor != null && persistor.hasNextIndividualAction then
            persistor.nextIndividualAction
          else
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

            if persistor != null then persistor.writeIndividualAction(chosenAction)
            chosenAction
          end action
          
          if action != -1 then
            val theAction = actions(action)
            ind.recordAction(theAction)
            theAction.apply(this, x, y)
            actionCount(action) += 1
          end if

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
  
  private inline def withIteration[T](index: Long)(inline body: => T): T =
    checkNotClosed()
    currentFrameRandom = baseRandom.copyAndJump()
    if persistor != null then persistor.startIteration()
    try
      isOutsideIterationProper = false
      body
    finally
      isOutsideIterationProper = true
      if persistor != null then persistor.finishIteration()
  
  /**
   * Initializes the field randomly as configured.
   */
  private def initialize(): Unit = withIteration(0):
    loopFromUntil(0, field.height): y =>
      loopFromUntil(0, field.width): x =>
        val cell = field.getSquare(x, y)
        cell.eraseEverything()
        cell.setFood(1e-9)
        if currentFrameRandom.nextDouble() < config.initialBacteriaProbability then
          val genome = IArray.tabulate(config.initialGenomeLength)(i => randomInstruction(i))
          val individual = createBacterium(genome, 0, config.initialHealth, currentFrameRandom.nextInt(4), null)
          cell.setIndividual(individual)
          nAliveBacteria += 1
  
  /**
   * Performs a single simulation step and returns the statistics computed after performing the step.
   * @return the statistics for the step just performed.
   */
  def simulationStep(): StepStatistics = withIteration(nIterationsPerformed + 1):
    nIterationsPerformed += 1
    val actionCount = performActionsOnIndividuals()
    drainIdleEnergy()
    depositFoodAndConvertDebris()
    computeStatistics(actionCount)
  
  /**
   * Returns whether simulation can continue.
   * @return whether simulation can continue.
   */
  def canContinue: Boolean = nAliveBacteria > 0 && (persistor == null || persistor.isWritable || persistor.hasMoreIterations)
  
  /**
   * Closes this simulation and releases all associated resources.
   */
  override def close(): Unit =
    isClosed = true
    if persistor != null then persistor.close()

object Simulation:
  /**
   * This creates a new simulation given the (prototype) configuration and creates all other resources on its own.
   * The prototype configuration is different from the actual configuration to be used in that
   * the random seed may be set to 0, in which case the new (time-based) seed will be generated.
   *
   * An optional argument is the persistor factory,
   *
   * @param protoConfig the prototype configuration to use.
   * @param persistorFactory the (optional) persistor factory to use.
   */
  def apply(protoConfig: Config, persistorFactory: Option[PersistorFactory]): Simulation =
    val config = protoConfig.withFixedSeed
    val result = new Simulation(
      config = config,
      field = Field(config.fieldWidth, config.fieldHeight),
      baseRandom = RandomGeneratorFactory.of[JumpableGenerator](config.randomFactory).create(config.randomSeed),
      persistor = persistorFactory match
        case None => null
        case Some(f) => f.connect(config)
    )

    println(s"Runtime context created with random factory '${config.randomFactory}' and ${
      if protoConfig.randomSeed == 0
      then s"NEW time-based seed ${config.randomSeed}"
      else s"fixed seed ${config.randomSeed}"
    }")
    
    result

  trait GenealogyListener:
    def bacteriumBorn(individual: Individual, parent: Individual, iteration: Long): Unit
    def bacteriumDead(individual: Individual, iteration: Long): Unit
    