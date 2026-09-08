package alife

import alife.util.Loops

import java.awt.Graphics
import java.awt.image.BufferedImage
import javax.swing.JPanel

class FieldVisualizer(field: Field, pixelScale: Int) extends JPanel:
  private final val widthInPixels = field.width * pixelScale
  private final val heightInPixels = field.height * pixelScale
  private final val pixels = Array.ofDim[Int](widthInPixels * heightInPixels)

  private var maxDebris, maxHealth, maxFood = 0.0
  private val image = BufferedImage(widthInPixels, heightInPixels, BufferedImage.TYPE_INT_ARGB)
  private var magentaLabel = -1

  private def visualConversion(a: Double): Double = math.log1p(a * (math.E - 1))

  def setMagentaLabel(newMagentaLabel: Int): Unit = magentaLabel = newMagentaLabel

  def translate(x: Int, y: Int): (Int, Int) = (x / pixelScale, y / pixelScale)

  def resetState(): Unit =
    maxDebris = 0
    maxHealth = 0
    maxFood = 0

  def fetchField(): Unit =
    maxDebris *= 0.95
    maxHealth *= 0.95
    maxFood *= 0.95

    val height = field.height
    val width = field.width

    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val cell = field.getCell(x, y)
        maxDebris = math.max(maxDebris, cell.debris)
        maxHealth = math.max(maxHealth, cell.health)
        maxFood = math.max(maxFood, cell.food)

    Loops.foreach(0, height): y =>
      Loops.foreach(0, width): x =>
        val cell = field.getCell(x, y)
        val g = cell.individual
        val z = if g != null && g.label == magentaLabel then 0xffff00ff else
          val d = (visualConversion(cell.debris / maxDebris) * 255).toInt
          val h = (visualConversion(cell.health / maxHealth) * 255).toInt
          val e = (visualConversion(cell.food / maxFood) * 255).toInt
          (h << 16) | (e << 8) | d | 0xff000000
        Loops.foreach(0, pixelScale): dy =>
          Loops.foreach(0, pixelScale): dx =>
            pixels(pixelScale * x + dx + (pixelScale * y + dy) * widthInPixels) = z

    image.synchronized(image.setRGB(0, 0, widthInPixels, heightInPixels, pixels, 0, widthInPixels))
    repaint()

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    image.synchronized(g.drawImage(image, 0, 0, this))
