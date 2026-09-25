package alife

import alife.Action.*
import alife.Instruction.RandomFactory
import alife.util.ChecksumSupport

import java.util.random.RandomGenerator
import java.util.{Properties, StringTokenizer}

case class Config(fieldWidth: Int, fieldHeight: Int,
                  randomSeed: Long, randomFactory: String,
                  initialGenomeLength: Int, initialBacteriaProbability: Double, initialHealth: Double,
                  rotationCost: Double, moveCost: Double, eatCost: Double, forkCost: Double,
                  debrisDegradation: Double, debrisToFood: Double, debrisFromActions: Double,
                  synthesisInit: Double, synthesisFinal: Double, synthesisDecay: Double, synthesisRandomness: Double,
                  idleCost: Double, healthMultiple: Double, healthIncrementMultiple: Double,
                  spotPeriodX: Double, spotSpeedX: Double, spotPeriodY: Double, spotSpeedY: Double, spotDecay: Double,
                  actionSequence: Seq[Action], visionStrength: Int,
                  instructionSequence: Seq[Instruction.RandomFactory], instructionProbabilities: Seq[Double]):
  require(0 <= visionStrength, "Vision strength should be non-negative")
  require(visionStrength < Field.numberOfRelativeLocations, s"Vision strength should be less than ${Field.numberOfRelativeLocations}")

  /**
   * This is a text representation of the config's contents, which is used both for checksums and for exports.
   */
  private lazy val textRepresentation = Config.SupportV1.computeTextRepresentation(this)
  
  /**
   * @inheritdoc
   */
  override lazy val toString: String = "Config(" + String(textRepresentation).init.replace("\n", ", ") + ")"
  
  /**
   * List of all actions as `IArray` for performance.
   */
  val actions: IArray[Action] = IArray(actionSequence *)
  
  private val instructions: IArray[Instruction.RandomFactory] = IArray(instructionSequence *)
  private val instructionPrefixSum: IArray[Double] = IArray.unsafeFromArray(instructionProbabilities.scanLeft(0.0)(_ + _).toArray)
  
  assert(math.abs(instructionPrefixSum.last - 1.0) < 1e-13)
  
  /**
   * Samples a random instruction factory, given the random generator.
   * @param random the random generator to use.
   * @return the sampled instruction factory.
   */
  def randomInstructionFactory(random: RandomGenerator): Instruction.RandomFactory =
    val number = random.nextDouble()
    var left = 0
    var right = instructionPrefixSum.length
    while right - left > 1 do
      val mid = (left + right) >>> 1
      if instructionPrefixSum(mid) > number
      then right = mid
      else left = mid
    instructions(left)
  
  /**
   * Returns the config which is identical to this one, except that if `randomSeed` is zero, it is set to the current time.
   * @return the copy of this config with a non-zero random seed.
   */
  def withFixedSeed: Config =
    if randomSeed != 0
    then this
    else copy(randomSeed = System.nanoTime())
  
  /**
   * The checksum of the config. This is computed using the MD5sum algorithm.
   * This is not intended to be cryptographically secure, we just protect against accidental mismatches.
   */
  lazy val checksum: String =
    require(randomSeed != 0, "Checksums make no sense for prototype configurations (with random seed == 0)")
    ChecksumSupport.md5sum(textRepresentation).asString
  
  /**
   * Returns the canonical byte array representation of this config.
   * @return the canonical byte array representation.
   */
  def toByteArray: Array[Byte] =
    require(randomSeed != 0, "Exporting to Array[Byte] makes no sense for prototype configurations (with random seed == 0)")
    val checkSumComponent = s"lifeConfigChecksum = $checksum\n".getBytes
    val result = Array.ofDim[Byte](textRepresentation.length + checkSumComponent.length)
    System.arraycopy(textRepresentation, 0, result, 0, textRepresentation.length)
    System.arraycopy(checkSumComponent, 0, result, textRepresentation.length, checkSumComponent.length)
    result

object Config:
  private object SupportV1:
    def computeTextRepresentation(config: Config): Array[Byte] =
      s"""lifeConfigVersion = 1
         |fieldWidth = ${config.fieldWidth}
         |fieldHeight = ${config.fieldHeight}
         |randomSeed = ${config.randomSeed}
         |randomFactory = ${config.randomFactory}
         |initialGenomeLength = ${config.initialGenomeLength}
         |initialBacteriaProbability = ${config.initialBacteriaProbability}
         |initialHealth = ${config.initialHealth}
         |rotationCost = ${config.rotationCost}
         |moveCost = ${config.moveCost}
         |eatCost = ${config.eatCost}
         |forkCost = ${config.forkCost}
         |debrisDegradation = ${config.debrisDegradation}
         |debrisToFood = ${config.debrisToFood}
         |debrisFromActions = ${config.debrisFromActions}
         |synthesisInit = ${config.synthesisInit}
         |synthesisFinal = ${config.synthesisFinal}
         |synthesisDecay = ${config.synthesisDecay}
         |synthesisRandomness = ${config.synthesisRandomness}
         |idleCost = ${config.idleCost}
         |healthMultiple = ${config.healthMultiple}
         |healthIncrementMultiple = ${config.healthIncrementMultiple}
         |spotPeriodX = ${config.spotPeriodX}
         |spotSpeedX = ${config.spotSpeedX}
         |spotPeriodY = ${config.spotPeriodY}
         |spotSpeedY = ${config.spotSpeedY}
         |spotDecay = ${config.spotDecay}
         |actionSequence = ${config.actionSequence.mkString(", ")}
         |visionStrength = ${config.visionStrength}
         |instructions = ${config.instructionSequence.mkString(", ")}
         |${config.instructionSequence.indices
              .map(i => s"instructionProbability.${config.instructionSequence(i)} = ${config.instructionProbabilities(i)}")
              .mkString("\n")}
         |""".stripMargin.getBytes
  
    def parse(properties: Properties, forceCheckIntegrity: Boolean): Config =
      val actionSequenceSource = StringTokenizer(properties.getProperty("actionSequence"), " ,")
      val actionSequence = IndexedSeq.fill[alife.Action](actionSequenceSource.countTokens()):
        actionSequenceSource.nextToken() match
          case s"Fork($operator)" => Fork:
            operator match
              case s"Primitive($prob)" => Mutation.Primitive(prob.toDouble)
              case s"Smooth($prob)" => Mutation.Smooth(prob.toDouble)
              case "NoChange" => Mutation.NoChange
              case other => throw IllegalArgumentException(s"In 'actionSequence', unknown mutation operator '$other'")
          case "Move" => Move
          case "Eat" => Eat
          case "RotatePlus" => RotatePlus
          case "RotateMinus" => RotateMinus
          case other => throw IllegalArgumentException(s"In 'actionSequence', unknown action '$other'")
      if actionSequence.distinct.size != actionSequence.size then
        throw IllegalArgumentException("Repeated elements in 'actionSequence'")
      
      val seed = properties.getProperty("randomSeed", "0").toLong
      val randomFactory = properties.getProperty("randomFactory")
      require(randomFactory != null, "Property 'randomFactory' not specified! This must name a jumpable random number generator factory. Use 'Xoshiro256PlusPlus' if unsure")
      
      val instructionSequenceSource = StringTokenizer(properties.getProperty("instructions"), " ,")
      val instructionSequence = IndexedSeq.fill(instructionSequenceSource.countTokens())(Instruction.factoryByName(instructionSequenceSource.nextToken())).sortBy(_.toString)
      if instructionSequence.distinct.size != instructionSequence.size then
        throw IllegalArgumentException("Duplicate elements in 'instructionSequence'")
      
      val instructionProbabilities =
        // Attempt 1: load stuff from "instructionProbability.XXX"
        val attempt1 = instructionSequence.map: f =>
          val propName = s"instructionProbability.$f"
          if properties.containsKey(propName) then
            properties.getProperty(propName).toDoubleOption match
              case None => throw IllegalArgumentException(s"Probability '$propName' is not a 'Double'")
              case Some(v) =>
                if !(0 <= v && v <= 1) then throw IllegalArgumentException(s"Probability '$propName' is not in [0;1]")
                Some(v)
          else None
        if attempt1.forall(_.isDefined) then
          val result = attempt1.map(_.get)
          val sum = result.sum
          if math.abs(sum - 1) > 1e-14 then throw IllegalArgumentException("All 'instructionProbability's do not sum up to 1")
          result
        else
          // Attempt 2: load stuff from "instructionWeight.XXX" and normalize
          val attempt2 = instructionSequence.map: f =>
            val propName = s"instructionWeight.$f"
            val theValue = properties.getProperty(propName).toDouble
            if theValue < 0 then throw IllegalArgumentException(s"Weight $propName is negative")
            if !theValue.isFinite then throw IllegalArgumentException(s"Weight $propName is infinite or NaN")
            theValue
          val sum = attempt2.sum
          if sum == 0 then throw IllegalArgumentException("All 'instructionWeight's are zero")
          attempt2.map(_ / sum)
      
      val result = Config(
        fieldWidth = properties.getProperty("fieldWidth").toInt,
        fieldHeight = properties.getProperty("fieldHeight").toInt,
        randomSeed = seed,
        randomFactory = randomFactory,
        initialGenomeLength = properties.getProperty("initialGenomeLength").toInt,
        initialBacteriaProbability = properties.getProperty("initialBacteriaProbability").toDouble,
        initialHealth = properties.getProperty("initialHealth").toDouble,
        rotationCost = properties.getProperty("rotationCost").toDouble,
        moveCost = properties.getProperty("moveCost").toDouble,
        eatCost = properties.getProperty("eatCost").toDouble,
        forkCost = properties.getProperty("forkCost").toDouble,
        debrisDegradation = properties.getProperty("debrisDegradation").toDouble,
        debrisToFood = properties.getProperty("debrisToFood").toDouble,
        debrisFromActions = properties.getProperty("debrisFromActions").toDouble,
        synthesisInit = properties.getProperty("synthesisInit").toDouble,
        synthesisFinal = properties.getProperty("synthesisFinal").toDouble,
        synthesisDecay = properties.getProperty("synthesisDecay").toDouble,
        synthesisRandomness = properties.getProperty("synthesisRandomness").toDouble,
        idleCost = properties.getProperty("idleCost").toDouble,
        healthMultiple = properties.getProperty("healthMultiple").toDouble,
        healthIncrementMultiple = properties.getProperty("healthIncrementMultiple").toDouble,
        spotPeriodX = properties.getProperty("spotPeriodX").toDouble,
        spotPeriodY = properties.getProperty("spotPeriodY").toDouble,
        spotSpeedX = properties.getProperty("spotSpeedX").toDouble,
        spotSpeedY = properties.getProperty("spotSpeedY").toDouble,
        spotDecay = properties.getProperty("spotDecay").toDouble,
        actionSequence = actionSequence,
        visionStrength = properties.getProperty("visionStrength").toInt,
        instructionSequence = instructionSequence,
        instructionProbabilities = instructionProbabilities,
      )
      
      val integrity = properties.getProperty("lifeConfigChecksum")
      if forceCheckIntegrity && integrity == null then
        throw IllegalArgumentException("Config integrity check forced, but 'lifeConfigChecksum' is not set")
      if integrity != null && integrity != result.checksum then
        throw IllegalArgumentException("Checksums do not match")
      
      result
  
  def parse(properties: Properties, forceCheckIntegrity: Boolean = false): Config =
    properties.getProperty("lifeConfigVersion", "1") match
      case "1" => SupportV1.parse(properties, forceCheckIntegrity)
      case other => throw IllegalArgumentException(s"Unknown value for 'lifeConfigVersion': '$other'")
