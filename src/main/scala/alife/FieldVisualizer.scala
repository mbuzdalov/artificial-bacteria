package alife

import alife.FieldVisualizer.StateBuffers
import alife.util.{DelayGate, Loops}

import java.awt.Graphics
import java.awt.image.BufferedImage
import javax.swing.JPanel

class FieldVisualizer(field: Field, pixelScale: Int, delayGate: DelayGate) extends JPanel:
  private final val widthInPixels = field.width * pixelScale
  private final val heightInPixels = field.height * pixelScale
  private final val pixels = Array.ofDim[Int](widthInPixels * heightInPixels)

  private val image = BufferedImage(widthInPixels, heightInPixels, BufferedImage.TYPE_INT_ARGB)
  private var magentaLabel = -1
  private val buffers = StateBuffers(field.width, field.height)
  
  private val painter = new Runnable:
    override def run(): Unit =
      Loops.forever:
        delayGate.runOrWait:
          buffers.acquireReadBuffer().upload(image, pixelScale, pixels, FieldVisualizer.this)
          buffers.releaseReadBuffer()
  private val thread = Thread(painter, "Field Visualizer Painter Thread")
  thread.setDaemon(true)
  thread.start()
  
  def setMagentaLabel(newMagentaLabel: Int): Unit = magentaLabel = newMagentaLabel

  def translate(x: Int, y: Int): (Int, Int) = (x / pixelScale, y / pixelScale)

  def resetState(): Unit =
    buffers.acquireWriteBuffer().reset()
    buffers.releaseWriteBuffer()

  def fetchField(): Unit =
    buffers.acquireWriteBuffer().fetch(field, magentaLabel)
    buffers.releaseWriteBuffer()

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    image.synchronized(g.drawImage(image, 0, 0, this))

object FieldVisualizer:
  private class StateBuffer(w: Int, h: Int):
    private var maxDebris, maxHealth, maxFood = 0.0
    private val dhfSequence = Array.ofDim[Double](w * h * 3)
    private val magSequence = Array.ofDim[Boolean](w * h)
    
    private def visualConversion(a: Double): Double = math.log1p(a * (math.E - 1))
    
    def reset(): Unit =
      maxDebris = 0.0
      maxHealth = 0.0
      maxFood = 0.0
    
    def fetch(field: Field, magentaLabel: Int): Unit =
      require(field.width == w)
      require(field.height == h)

      maxDebris *= 0.95
      maxHealth *= 0.95
      maxFood *= 0.95
      
      var idx = 0
      Loops.foreach(0, field.height): y =>
        Loops.foreach(0, field.width): x =>
          val cell = field.getCell(x, y)
          val ind = cell.individual
          val i3 = idx * 3
          dhfSequence(i3) = cell.debris
          dhfSequence(i3 + 1) = cell.health
          dhfSequence(i3 + 2) = cell.food
          magSequence(idx) = ind != null && ind.label == magentaLabel
          idx += 1
          maxDebris = math.max(maxDebris, cell.debris)
          maxHealth = math.max(maxHealth, cell.health)
          maxFood = math.max(maxFood, cell.food)

    def upload(image: BufferedImage, pixelScale: Int, pixels: Array[Int], component: JPanel): Unit =
      val widthInPixels = w * pixelScale
      val heightInPixels = h * pixelScale
      var idx = 0
      Loops.foreach(0, h): y =>
        Loops.foreach(0, w): x =>
          val i3 = idx * 3
          val z = if magSequence(idx) then 0xffff00ff else
            val d = (visualConversion(dhfSequence(i3) / maxDebris) * 255).toInt
            val h = (visualConversion(dhfSequence(i3 + 1) / maxHealth) * 255).toInt
            val e = (visualConversion(dhfSequence(i3 + 2) / maxFood) * 255).toInt
            (h << 16) | (e << 8) | d | 0xff000000
          idx += 1

          Loops.foreach(0, pixelScale): dy =>
            Loops.foreach(0, pixelScale): dx =>
              pixels(pixelScale * x + dx + (pixelScale * y + dy) * widthInPixels) = z
      
      image.synchronized(image.setRGB(0, 0, widthInPixels, heightInPixels, pixels, 0, widthInPixels))
      component.repaint()

  private class StateBuffers(w: Int, h: Int):
    private var readBuffer, writeBuffer, doneBuffer = StateBuffer(w, h)
    private var readBufferBusy, anyChanges: Boolean = false

    def acquireReadBuffer(): StateBuffer = synchronized:
      assert(!readBufferBusy)
      while !anyChanges do
        wait()
      readBufferBusy = true
      if anyChanges then
        anyChanges = false
        val tmp = doneBuffer
        doneBuffer = readBuffer
        readBuffer = tmp
      readBuffer
      
    def releaseReadBuffer(): Unit = synchronized:
      assert(readBufferBusy)
      readBufferBusy = false
      
    def acquireWriteBuffer(): StateBuffer = synchronized(writeBuffer)
    
    def releaseWriteBuffer(): Unit = synchronized:
      anyChanges = true
      notify()
      val tmp = doneBuffer
      doneBuffer = writeBuffer
      writeBuffer = tmp
