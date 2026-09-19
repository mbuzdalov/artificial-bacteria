package alife

import alife.Action.*
import alife.util.ChecksumSupport

import java.util.{Properties, StringTokenizer}
import java.io.OutputStream

case class Config(fieldWidth: Int, fieldHeight: Int,
                  randomSeed: Long, randomFactory: String,
                  initialGenomeLength: Int, initialBacteriaProbability: Double, initialHealth: Double,
                  rotationCost: Double, moveCost: Double, eatCost: Double, forkCost: Double,
                  debrisDegradation: Double, debrisToFood: Double, debrisFromActions: Double,
                  synthesisInit: Double, synthesisFinal: Double, synthesisDecay: Double,
                  idleCost: Double, healthMultiple: Double, healthIncrementMultiple: Double,
                  spotPeriodX: Double, spotSpeedX: Double, spotPeriodY: Double, spotSpeedY: Double, spotDecay: Double,
                  actionSequence: Seq[Action]):
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
   * Appends the (textual representation of) this config to the given output stream.
   * This will fail if this is a prototype config (with a zero random seed).
   * @param stream the output stream to append to.
   */
  def exportToStream(stream: OutputStream): Unit =
    require(randomSeed != 0, "Exporting to streams makes no sense for prototype configurations (with random seed == 0)")
    stream.write(textRepresentation)
    val checkSumComponent =
      s"""lifeConfigChecksum = $checksum
         |""".stripMargin
    stream.write(checkSumComponent.getBytes)

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
         |idleCost = ${config.idleCost}
         |healthMultiple = ${config.healthMultiple}
         |healthIncrementMultiple = ${config.healthIncrementMultiple}
         |spotPeriodX = ${config.spotPeriodX}
         |spotSpeedX = ${config.spotSpeedX}
         |spotPeriodY = ${config.spotPeriodY}
         |spotSpeedY = ${config.spotSpeedY}
         |spotDecay = ${config.spotDecay}
         |actionSequence = ${config.actionSequence.mkString(", ")}
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
        idleCost = properties.getProperty("idleCost").toDouble,
        healthMultiple = properties.getProperty("healthMultiple").toDouble,
        healthIncrementMultiple = properties.getProperty("healthIncrementMultiple").toDouble,
        spotPeriodX = properties.getProperty("spotPeriodX").toDouble,
        spotPeriodY = properties.getProperty("spotPeriodY").toDouble,
        spotSpeedX = properties.getProperty("spotSpeedX").toDouble,
        spotSpeedY = properties.getProperty("spotSpeedY").toDouble,
        spotDecay = properties.getProperty("spotDecay").toDouble,
        actionSequence = actionSequence
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
