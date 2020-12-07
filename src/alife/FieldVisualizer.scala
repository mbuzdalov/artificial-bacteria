package alife

import java.awt.Graphics
import java.awt.image.BufferedImage
import javax.swing.JPanel

class FieldVisualizer(field: Field, pixelScale: Int) extends JPanel {
  private[this] final val widthInPixels = field.width * pixelScale
  private[this] final val heightInPixels = field.height * pixelScale
  private[this] final val pixels = Array.ofDim[Int](widthInPixels * heightInPixels)

  private[this] var maxHealth = 0.0
  private[this] var maxEnergy = 0.0
  private[this] val image = new BufferedImage(widthInPixels, heightInPixels, BufferedImage.TYPE_INT_ARGB)

  private def visualConversion(a: Double): Double = math.log1p(a * (math.E - 1))

  def translate(x: Int, y: Int): (Int, Int) = (x / pixelScale, y / pixelScale)

  def fetchField(): Unit = {
    maxHealth *= 0.95
    maxEnergy *= 0.95

    val height = field.height
    val width = field.width

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
        while (dx < pixelScale) {
          var dy = 0
          while (dy < pixelScale) {
            pixels(pixelScale * x + dx + (pixelScale * y + dy) * widthInPixels) = z
            dy += 1
          }
          dx += 1
        }
        x += 1
      }
      y += 1
    }
    image.synchronized {
      image.setRGB(0, 0, widthInPixels, heightInPixels, pixels, 0, widthInPixels)
    }
    repaint()
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    image.synchronized {
      g.drawImage(image, 0, 0, this)
    }
  }
}
