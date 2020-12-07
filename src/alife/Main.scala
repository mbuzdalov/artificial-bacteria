package alife

import java.awt.{BorderLayout, Graphics}
import java.awt.image.BufferedImage
import java.io.FileReader
import java.util.concurrent.ThreadLocalRandom
import java.util.Properties

import javax.swing._

import scala.annotation.tailrec

/**
 * A first attempt to run the bacteria system
 */
object Main {
  private def visualConversion(a: Double): Double = math.log1p(a * (math.E - 1))

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
    val viewScale = properties.getProperty("pixelScale").toInt
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

    object View extends JPanel {
      final val widthInPixels = width * viewScale
      final val heightInPixels = height * viewScale
      final val pixels = Array.ofDim[Int](widthInPixels * heightInPixels)

      private[this] val image = new BufferedImage(widthInPixels, heightInPixels, BufferedImage.TYPE_INT_ARGB)

      def refill(): Unit = {
        image.setRGB(0, 0, widthInPixels, heightInPixels, pixels, 0, widthInPixels)
      }

      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        g.drawImage(image, 0, 0, View)
      }
    }

    var maxHealth = 100.0
    var maxEnergy = 100.0

    def fillImage(): Unit = {
      maxHealth *= 0.95
      maxEnergy *= 0.95

      var y = 0
      while (y < height) {
        var x = 0
        while (x < width) {
          maxHealth = math.max(maxHealth, field.getHealth(x, y))
          maxEnergy = math.max(maxEnergy, field.getEnergy(x, y))
          maxEnergy = math.max(maxEnergy, field.getDebris(x, y))
          x += 1
        }
        y += 1
      }
      y = 0
      while (y < height) {
        var x = 0
        while (x < width) {
          val h = (visualConversion(field.getHealth(x, y) / maxHealth) * 255).toInt
          val e = (visualConversion(field.getEnergy(x, y) / maxEnergy) * 255).toInt
          val d = (visualConversion(field.getDebris(x, y) / maxEnergy) * 255).toInt
          val z = (h << 16) | (e << 8) | d | 0xff000000
          var dx = 0
          while (dx < viewScale) {
            var dy = 0
            while (dy < viewScale) {
              View.pixels(viewScale * x + dx + (viewScale * y + dy) * View.widthInPixels) = z
              dy += 1
            }
            dx += 1
          }
          x += 1
        }
        y += 1
      }
      View.refill()
      View.repaint()
    }

    val window = new JFrame("Bacteria")
    window.setLayout(new BorderLayout())
    window.add(View, BorderLayout.CENTER)
    window.pack()
    window.setVisible(true)

    @tailrec
    def work(generation: Int): Unit = {
      fillImage()
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
