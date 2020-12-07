package alife

import java.awt.{BorderLayout, Graphics}
import java.awt.image.BufferedImage
import java.util.concurrent.ThreadLocalRandom
import javax.swing._
import alife.Field.Constants

import scala.annotation.tailrec

/**
 * A first attempt to run the bacteria system
 */
object Main {
  def main(args: Array[String]) {
    val constants = Constants(
      rotationCost = 0.01,      // what portion of current effective weight is spent on rotation
      moveCost = 0.02,          // what portion of current effective weight is spend on move
      eatCost = 0.05,           // what part of food is not actually eaten
      forkCost = 0.1,           // what part of chromosome weight, as energy, is spend on forking
      debrisDegradation = 0.02, // what part of debris disappear
      debrisToEnergy = 0.0,     // what part of debris becomes accessible as energy again
      synthesisInit = 0.3,      // the expected amount of new food to be generated on first generations
      synthesisFinal = 0.03,    // the expected amount of new food to be generated on infinitely remote generations
      synthesisDecay = 0.001,   // how fast the amount of new food moves from first to infinitely remove generations
      idleCost = 0.1,           // the amount of energy to spend on just being alive
      healthMultiple = 10,      // how much energy can a bacteria keep, relative to genome size
      spotPeriodX = 2,          // how many hot spots (in terms of food) in a row will eventually appear
      spotPeriodY = 1,          // how many how spots in a column will eventually appear
      spotSpeedX = 0.000094,    // how many horizontal space per generation every hot spot will move
      spotSpeedY = 0.0002,      // how many vertical space per generation every hot spot will move
      spotDecay = 0.01          // how fast everything outside the spot will decay
    )

    val width = 957
    val height = 520
    val viewScale = 2
    val initHealth = 10
    val genomeLength = 50
    val field = new Field(width, height)

    var y = 0
    while (y < height) {
      var x = 0
      while (x < width) {
        if (ThreadLocalRandom.current().nextInt(5) == 0) {
          field.setGenome(x, y, IndexedSeq.tabulate(genomeLength)(Instruction.random), ThreadLocalRandom.current().nextInt(4), initHealth)
        }
        x += 1
      }
      y += 1
    }

    object View extends JPanel {
      final val pixelWidth = width * viewScale
      final val pixelHeight = height * viewScale
      final val pixels = Array.ofDim[Int](pixelWidth * pixelHeight)

      private[this] val image = new BufferedImage(pixelWidth, pixelHeight, BufferedImage.TYPE_INT_ARGB)

      def refill(): Unit = {
        image.setRGB(0, 0, pixelWidth, pixelHeight, pixels, 0, pixelWidth)
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
          val h = (field.getHealth(x, y) * 255 / maxHealth).toInt
          val e = (field.getEnergy(x, y) * 255 / maxEnergy).toInt
          val d = (field.getDebris(x, y) * 255 / maxEnergy).toInt
          val z = (h << 16) | (e << 8) | d | 0xff000000
          var dx = 0
          while (dx < viewScale) {
            var dy = 0
            while (dy < viewScale) {
              View.pixels(viewScale * x + dx + (viewScale * y + dy) * View.pixelWidth) = z
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
