package alife

import alife.Action.*

import java.util.random.{RandomGenerator, RandomGeneratorFactory}
import java.util.{Properties, StringTokenizer}

case class Config(fieldWidth: Int, fieldHeight: Int,
                  randomSeed: Long, randomFactory: String,
                  initialGenomeLength: Int, initialBacteriaProbability: Double, initialHealth: Double,
                  rotationCost: Double, moveCost: Double, eatCost: Double, forkCost: Double,
                  debrisDegradation: Double, debrisToFood: Double, debrisFromActions: Double,
                  synthesisInit: Double, synthesisFinal: Double, synthesisDecay: Double,
                  idleCost: Double, healthMultiple: Double, healthIncrementMultiple: Double,
                  spotPeriodX: Double, spotSpeedX: Double, spotPeriodY: Double, spotSpeedY: Double, spotDecay: Double,
                  actions: IArray[Action]):
  val random: RandomGenerator = RandomGeneratorFactory.of(randomFactory).create(randomSeed)

object Config:
  def parse(properties: Properties): Config =
    val actionSequenceSource = StringTokenizer(properties.getProperty("actionSequence"), " ,")
    val actionSequence = IArray.fill[alife.Action](actionSequenceSource.countTokens()):
      actionSequenceSource.nextToken() match
        case s"Fork($operator)" => Fork:
          operator match
            case "Primitive" => Mutation.Primitive
            case "Smooth" => Mutation.Smooth
        case "Move" => Move
        case "Eat" => Eat
        case "RotatePlus" => RotatePlus
        case "RotateMinus" => RotateMinus
        case other => throw IllegalArgumentException(s"In 'actionSequence', unknown action '$other'")
    if actionSequence.distinct.size != actionSequence.size then
      throw IllegalArgumentException("Repeated elements in 'actionSequence'")
    
    val seed = properties.getProperty("randomSeed", System.nanoTime().toString).toLong
    val randomFactory = properties.getProperty("randomFactory", RandomGeneratorFactory.getDefault.name())
    
    Config(
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
      actions = actionSequence
    )
