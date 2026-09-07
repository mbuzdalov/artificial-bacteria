package alife

import alife.util.Loops

import java.awt.Graphics
import java.awt.image.BufferedImage
import javax.swing.JPanel

class FieldVisualizer(field: Field, pixelScale: Int) extends JPanel:
  private final val widthInPixels = field.width * pixelScale
  private final val heightInPixels = field.height * pixelScale
  private final val pixels = Array.ofDim[Int](widthInPixels * heightInPixels)

  private var maxDebris = 0.0
  private var maxHealth = 0.0
  private var maxEnergy = 0.0
  private val image = BufferedImage(widthInPixels, heightInPixels, BufferedImage.TYPE_INT_ARGB)
  private var magentaLabel = -1

  private def visualConversion(a: Double): Double = math.log1p(a * (math.E - 1))

  def setMagentaLabel(newMagentaLabel: Int): Unit = magentaLabel = newMagentaLabel

  def translate(x: Int, y: Int): (Int, Int) = (x / pixelScale, y / pixelScale)

  def resetState(): Unit =
    maxHealth = 0
    maxEnergy = 0
    maxDebris = 0

  def fetchField(): Unit =
    maxHealth *= 0.95
    maxEnergy *= 0.95
    maxDebris *= 0.95

    val height = field.height
    val width = field.width

    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        maxHealth = math.max(maxHealth, field.getHealth(x, y))
        maxEnergy = math.max(maxEnergy, field.getEnergy(x, y))
        maxDebris = math.max(maxDebris, field.getDebris(x, y))

    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val g = field.getIndividual(x, y)
        val z = if g != null && g.label == magentaLabel then 0xffff00ff else
          val h = (visualConversion(field.getHealth(x, y) / maxHealth) * 255).toInt
          val e = (visualConversion(field.getEnergy(x, y) / maxEnergy) * 255).toInt
          val d = (visualConversion(field.getDebris(x, y) / maxDebris) * 255).toInt
          (h << 16) | (e << 8) | d | 0xff000000
        Loops.foreach(0, pixelScale): dy =>
          Loops.foreach(0, pixelScale): dx =>
            pixels(pixelScale * x + dx + (pixelScale * y + dy) * widthInPixels) = z

    image.synchronized(image.setRGB(0, 0, widthInPixels, heightInPixels, pixels, 0, widthInPixels))
    repaint()

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    image.synchronized(g.drawImage(image, 0, 0, this))
