package alife

import alife.Action.*
import java.util.{Properties, StringTokenizer}

case class Config(initialGenomeLength: Int, initialBacteriaProbability: Double, initialHealth: Double, 
                  rotationCost: Double, moveCost: Double, eatCost: Double, forkCost: Double,
                  debrisDegradation: Double, debrisToFood: Double, debrisFromActions: Double,
                  synthesisInit: Double, synthesisFinal: Double, synthesisDecay: Double,
                  idleCost: Double, healthMultiple: Double, healthIncrementMultiple: Double,
                  spotPeriodX: Double, spotSpeedX: Double, spotPeriodY: Double, spotSpeedY: Double,
                  spotDecay: Double, mutationOperator: Mutation,
                  actions: IArray[Action])

object Config:
  private class Lookup[T](pairs: (String, T)*):
    private val map = Map(pairs *)
    private lazy val expected = map.keys.map(k => s"'$k'").mkString(", ")
    
    def apply(arg: String, prefix: => String): T =
      map.getOrElse(arg, throw IllegalArgumentException(s"${prefix}Unknown value '$arg': expected one of $expected"))
    
    def apply(props: Properties, key: String): T =
      val value = props.getProperty(key)
      if value == null then throw IllegalArgumentException(s"No property '$key'")
      this.apply(value, s"For '$key': ")
  
  def parse(properties: Properties): Config =
    val globallyEnabledActions = Lookup(
      "Fork" -> Fork,
      "Move" -> Move,
      "Eat" -> Eat,
      "RotatePlus" -> RotatePlus,
      "RotateMinus" -> RotateMinus,
    )
    
    val globallyEnabledMutations = Lookup(
      "Primitive" -> Mutation.Primitive,
      "Smooth" -> Mutation.Smooth,
    )
    
    val actionSequenceSource = StringTokenizer(properties.getProperty("actionSequence"), " ,")
    val actionSequence = IArray.fill[alife.Action](actionSequenceSource.countTokens()):
      globallyEnabledActions(actionSequenceSource.nextToken(), "In 'actionSequence': ")
    if actionSequence.distinct.size != actionSequence.size then
      throw IllegalArgumentException("Repeated elements in 'actionSequence'")
    
    val mutationOperator = globallyEnabledMutations(properties, "mutationOperator")
    
    Config(
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
      mutationOperator = mutationOperator,
      actions = actionSequence
    )
