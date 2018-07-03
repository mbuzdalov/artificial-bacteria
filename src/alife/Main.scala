package alife

import java.awt.{BorderLayout, Graphics}
import java.awt.image.BufferedImage
import java.util.concurrent.ThreadLocalRandom
import javax.swing._

import alife.Field.Constants

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
      synthesis = 0.1,          // the expected amount of new food to be generated
      idleCost = 0.1,           // the amount of energy to spend on just being alive
      healthMultiple = 10       // how much energy can a bacteria keep, relative to genome size
    )

    val width = 450
    val height = 250
    val viewScale = 4
    val initHealth = 10
    val genomeLength = 50
    val field = new Field(width, height)

    var x = 0
    while (x < width) {
      var y = 0
      while (y < height) {
        if (ThreadLocalRandom.current().nextInt(5) == 0) {
          field.setGenome(x, y, IndexedSeq.tabulate(genomeLength)(Instruction.random), ThreadLocalRandom.current().nextInt(4), initHealth)
        }
        y += 1
      }
      x += 1
    }

    object View extends JPanel {
      val image = new BufferedImage(viewScale * width, viewScale * height, BufferedImage.TYPE_INT_RGB)

      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        g.drawImage(image, 0, 0, View)
      }
    }

    var maxHealth = 100.0
    var maxEnergy = 100.0
    var maxDebris = 100.0

    def fillImage(): Unit = {
      var x = 0
      while (x < width) {
        var y = 0
        while (y < height) {
          maxHealth = math.max(maxHealth, field.getHealth(x, y))
          maxEnergy = math.max(maxEnergy, field.getEnergy(x, y))
          maxDebris = math.max(maxDebris, field.getDebris(x, y))
          y += 1
        }
        x += 1
      }
      x = 0
      while (x < width) {
        var y = 0
        while (y < height) {
          val h = (field.getHealth(x, y) * 255 / maxHealth).toInt
          val e = (field.getEnergy(x, y) * 255 / maxEnergy).toInt
          val d = (field.getDebris(x, y) * 255 / maxDebris).toInt
          val z = (h << 16) | (e << 8) | d
          var dx = 0
          while (dx < viewScale) {
            var dy = 0
            while (dy < viewScale) {
              View.image.setRGB(viewScale * x + dx, viewScale * y + dy, z)
              dy += 1
            }
            dx += 1
          }
          y += 1
        }
        x += 1
      }
      View.repaint()
    }

    val window = new JFrame("Bacteria")
    window.setLayout(new BorderLayout())
    window.add(View, BorderLayout.CENTER)
    window.pack()
    window.setVisible(true)

    def work(generation: Int): Unit = {
      fillImage()
      println(s"Generation $generation")
      println(s"  Current bacteria number: ${field.getNumberOfBacteria}")
      println(s"  Current max genome size: ${field.getMaxGenomeSize}")
      if (field.getNumberOfBacteria > 0) {
        val actionsMade = field.simulationStep(constants)
        println(s"  Actions: $actionsMade")
        work(generation + 1)
      }
    }
    work(0)
  }
}
