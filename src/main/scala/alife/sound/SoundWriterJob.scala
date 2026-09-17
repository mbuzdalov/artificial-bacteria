package alife.sound

import alife.Field
import alife.util.Loops.*

import java.util.concurrent.atomic.AtomicReference
import javax.sound.sampled.{AudioFormat, AudioSystem}

/**
 * This takes a field being tracked and a `SoundSynthesizer` and produces sound
 * via Java's `AudioSystem`. The sound is produced in the 16-bit stereo format.
 * @param synthesizer the synthesizer to produce frames.
 * @param frequency the audio frame rate to use.
 * @param framesInBuffer the number of frames in the buffer.
 */
class SoundWriterJob(synthesizer: SoundSynthesizer, frequency: Float, framesInBuffer: Int) extends Runnable:
  require(framesInBuffer > 0, "There should be a positive number of frames in the buffer")
  private val myAudioFormat = AudioFormat(frequency, 16, 2, true, false)
  private val dataLine = AudioSystem.getSourceDataLine(myAudioFormat)
  private val field = AtomicReference[Field](null)

  def setField(newField: Field): Unit = field.set(newField)
  def clearField(): Unit = field.set(null)
  
  override def run(): Unit =
    val fieldSideBuffer = Array.ofDim[Double](2)
    var normalizationAmplitude = 1.0
    val buffer = Array.ofDim[Byte](4 * framesInBuffer)

    if !dataLine.isOpen then dataLine.open()
    dataLine.start()

    var nFrames = 0L
    loopForever:
      val f = field.get()
      if f == null 
      then java.util.Arrays.fill(buffer, 0.toByte)
      else loopFromUntil(0, framesInBuffer): i =>  
        synthesizer.synthesizeOneFrame(f, nFrames.toDouble / frequency, fieldSideBuffer)
        nFrames += 1
        val maxFieldValue = math.max(math.abs(fieldSideBuffer(0)), math.abs(fieldSideBuffer(1)))
        normalizationAmplitude = math.max(1, math.max(normalizationAmplitude / 1.00001, maxFieldValue))
        val leftValue = (fieldSideBuffer(0) / normalizationAmplitude * 32767).toInt
        val rightValue = (fieldSideBuffer(1) / normalizationAmplitude * 32767).toInt
        val off = i * 4
        buffer(off) = (leftValue & 0xff).toByte
        buffer(off + 1) = (leftValue >> 8).toByte
        buffer(off + 2) = (rightValue & 0xff).toByte
        buffer(off + 3) = (rightValue >> 8).toByte
      dataLine.write(buffer, 0, buffer.length)
