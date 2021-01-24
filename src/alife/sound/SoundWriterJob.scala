package alife.sound

import alife.Field

import javax.sound.sampled.{AudioFormat, AudioSystem}

class SoundWriterJob(field: Field, synthesizer: SoundSynthesizer, frequency: Int) extends Runnable {
  private val myAudioFormat = new AudioFormat(frequency.toFloat, 16, 2, true, false)
  private val dataLine = AudioSystem.getSourceDataLine(myAudioFormat)

  override def run(): Unit = {
    val fieldSideBuffer = new Array[Double](2)
    var normalizationAmplitude = 1.0
    val buffer = new Array[Byte](4)

    if (!dataLine.isOpen) {
      dataLine.open()
    }
    dataLine.start()

    var nFrames = 0L
    while (true) {
      synthesizer.synthesizeOneFrame(field, nFrames.toDouble / frequency, fieldSideBuffer)
      nFrames += 1
      val maxFieldValue = math.max(math.abs(fieldSideBuffer(0)), math.abs(fieldSideBuffer(1)))
      normalizationAmplitude = math.max(1, math.max(normalizationAmplitude / 1.00001, maxFieldValue))
      val leftValue = (fieldSideBuffer(0) / normalizationAmplitude * 32767).toInt
      val rightValue = (fieldSideBuffer(1) / normalizationAmplitude * 32767).toInt
      buffer(0) = (leftValue & 0xff).toByte
      buffer(1) = (leftValue >> 8).toByte
      buffer(2) = (rightValue & 0xff).toByte
      buffer(3) = (rightValue >> 8).toByte
      dataLine.write(buffer, 0, 4)
    }
  }
}
