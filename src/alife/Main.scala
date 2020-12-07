package alife

import java.awt.{BorderLayout, Frame}
import java.io.FileReader
import java.util.concurrent.ThreadLocalRandom
import java.util.Properties
import javax.swing._
import scala.annotation.tailrec

/**
 * A first attempt to run the bacteria system
 */
object Main {
  private def loadProperties(args: Array[String]): Properties = {
    val properties = new Properties()
    val fileName = if (args.length == 0) "config.properties" else args(0)
    val propReader = new FileReader(fileName)
    properties.load(propReader)
    propReader.close()
    properties
  }

  def main(args: Array[String]) {
    val properties = loadProperties(args)

    val constants = Field.Constants(
      rotationCost = properties.getProperty("rotationCost").toDouble,
      moveCost = properties.getProperty("moveCost").toDouble,
      eatCost = properties.getProperty("eatCost").toDouble,
      forkCost = properties.getProperty("forkCost").toDouble,
      debrisDegradation = properties.getProperty("debrisDegradation").toDouble,
      debrisToEnergy = properties.getProperty("debrisToEnergy").toDouble,
      synthesisInit = properties.getProperty("synthesisInit").toDouble,
      synthesisFinal = properties.getProperty("synthesisFinal").toDouble,
      synthesisDecay = properties.getProperty("synthesisDecay").toDouble,
      idleCost = properties.getProperty("idleCost").toDouble,
      healthMultiple = properties.getProperty("healthMultiple").toDouble,
      spotPeriodX = properties.getProperty("spotPeriodX").toDouble,
      spotPeriodY = properties.getProperty("spotPeriodY").toDouble,
      spotSpeedX = properties.getProperty("spotSpeedX").toDouble,
      spotSpeedY = properties.getProperty("spotSpeedY").toDouble,
      spotDecay = properties.getProperty("spotDecay").toDouble
    )

    val width = properties.getProperty("fieldWidth").toInt
    val height = properties.getProperty("fieldHeight").toInt
    val pixelScale = properties.getProperty("pixelScale").toInt
    val initialHealth = properties.getProperty("initialHealth").toDouble
    val initialGenomeLength = properties.getProperty("initialGenomeLength").toInt
    val initialBacteriaProbability = properties.getProperty("initialBacteriaProbability").toDouble
    val field = new Field(width, height)

    var y = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        if (ThreadLocalRandom.current().nextDouble( ) < initialBacteriaProbability) {
          field.setGenome(x, y, IndexedSeq.tabulate(initialGenomeLength)(Instruction.random), ThreadLocalRandom.current().nextInt(4), initialHealth)
        }
        x += 1
      }
      y += 1
    }

    val view = new FieldVisualizer(field, pixelScale)

    val window = new JFrame("Bacteria")
    window.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    window.setLayout(new BorderLayout())
    window.add(view, BorderLayout.CENTER)
    window.setExtendedState(Frame.MAXIMIZED_BOTH)
    window.setUndecorated(true)
    window.setVisible(true)

    @tailrec
    def work(generation: Int): Unit = if (window.isVisible) {
      view.fetchField()
      print(f"$generation: #bacteria: ${field.getNumberOfBacteria}%5d, max size: ${field.getMaxGenomeSize}%2d")
      if (field.getNumberOfBacteria > 0) {
        val actionsMade = field.simulationStep(constants, generation)
        for ((a, v) <- actionsMade) {
          if (v == v.toInt) {
            print(f", #$a: ${v.toInt}%4d")
          } else {
            print(f", $a: $v%10.2f")
          }
        }
        println()
        work(generation + 1)
      } else {
        println()
      }
    }
    work(0)
  }
}
