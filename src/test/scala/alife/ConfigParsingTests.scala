package alife

import alife.Action.{Eat, Fork, Move, RotateMinus, RotatePlus}
import alife.Mutation.{NoChange, Primitive, Smooth}
import alife.util.ChecksumSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.{ByteArrayOutputStream, StringReader}
import java.util.Properties
import scala.util.Using

class ConfigParsingTests extends AnyFlatSpec with should.Matchers:
  private val referenceV1Config = Config(
    fieldWidth = 239,
    fieldHeight = 42,
    randomSeed = 8234923843245L,
    randomFactory = "Xoshiro256PlusPlus",
    initialGenomeLength = 10,
    initialBacteriaProbability = 0.1,
    initialHealth = 10,
    rotationCost = 2,
    moveCost = 3,
    eatCost = 0.1,
    forkCost = 4,
    debrisDegradation = 0.2,
    debrisToFood = 0.01,
    debrisFromActions = 0.5,
    synthesisInit = 2,
    synthesisFinal = 0.02,
    synthesisDecay = 0.001,
    synthesisRandomness = 1.0,
    idleCost = 0.5,
    healthMultiple = 2,
    healthIncrementMultiple = 1,
    spotPeriodX = 3,
    spotSpeedX = 0.001,
    spotPeriodY = 2,
    spotSpeedY = 0.0027,
    spotDecay = 0.001,
    actionSequence = IndexedSeq(Fork(Smooth(1.0)), Fork(Primitive(0.1)), Fork(NoChange), Eat, Move, RotatePlus, RotateMinus),
  )
  private val referenceV1ConfigBase =
    """lifeConfigVersion = 1
      |fieldWidth = 239
      |fieldHeight = 42
      |randomSeed = 8234923843245
      |randomFactory = Xoshiro256PlusPlus
      |initialGenomeLength = 10
      |initialBacteriaProbability = 0.1
      |initialHealth = 10.0
      |rotationCost = 2.0
      |moveCost = 3.0
      |eatCost = 0.1
      |forkCost = 4.0
      |debrisDegradation = 0.2
      |debrisToFood = 0.01
      |debrisFromActions = 0.5
      |synthesisInit = 2.0
      |synthesisFinal = 0.02
      |synthesisDecay = 0.001
      |synthesisRandomness = 1.0
      |idleCost = 0.5
      |healthMultiple = 2.0
      |healthIncrementMultiple = 1.0
      |spotPeriodX = 3.0
      |spotSpeedX = 0.001
      |spotPeriodY = 2.0
      |spotSpeedY = 0.0027
      |spotDecay = 0.001
      |actionSequence = Fork(Smooth(1.0)), Fork(Primitive(0.1)), Fork(NoChange), Eat, Move, RotatePlus, RotateMinus
      |""".stripMargin
  private val referenceV1ConfigChecksum = ChecksumSupport.md5sum(referenceV1ConfigBase.getBytes).asString

  "Config.parse" should "parse V1 without a checksum correctly" in:
    val props = Properties()
    Using.resource(StringReader(referenceV1ConfigBase))(r => props.load(r))
    val cfg = Config.parse(props)
    cfg shouldEqual referenceV1Config
    cfg.checksum shouldEqual referenceV1ConfigChecksum
    
  it should "parse V1 with a correct checksum correctly" in:
    val props = Properties()
    Using.resource(StringReader(referenceV1ConfigBase))(r => props.load(r))
    props.setProperty("lifeConfigChecksum", referenceV1ConfigChecksum)
    val cfg = Config.parse(props)
    cfg shouldEqual referenceV1Config
    cfg.checksum shouldEqual referenceV1ConfigChecksum

  it should "fail to parse V1 with an incorrect checksum" in:
    val props = Properties()
    Using.resource(StringReader(referenceV1ConfigBase))(r => props.load(r))
    props.setProperty("lifeConfigChecksum", "2222")
    an[IllegalArgumentException] shouldBe thrownBy:
      Config.parse(props)

  "Config.exportToStream" should "produce identical results on the reference" in:
    val expected = s"${referenceV1ConfigBase}lifeConfigChecksum = $referenceV1ConfigChecksum\n"
    val stream = ByteArrayOutputStream()
    referenceV1Config.exportToStream(stream)
    val found = String(stream.toByteArray)
    found shouldEqual expected
