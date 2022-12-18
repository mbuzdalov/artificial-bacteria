package alife.sound

import alife.Field

object DefaultSynthesizer extends SoundSynthesizer {
  override def synthesizeOneFrame(field: Field, time: Double, target: Array[Double]): Unit = {
    // we want =8 octaves, each taking a multiple of 12 pixels = a multiple of 96
    val height = field.height
    val width = field.width
    val pixelsPerOctave = height / 8.0
    val baseFrequency = 345.57519189487726 // 55 Hz * 2 pi. 55 Hz = 440 / 8 corresponds to the lowest row of pixels
    val bacteriaPower = 0.01

    var y = 0
    var left, right = 0.0
    var note = Math.pow(2, (height - 1) / pixelsPerOctave) * baseFrequency * time
    val noteStep = Math.pow(2, 1 / pixelsPerOctave)
    while (y < height) {
      // a very left individual contributes a lot to distances from right...
      val localLeft = field.getSumOfDistancesFromRight(y)
      val localRight = field.getSumOfDistancesFromLeft(y)
      if (localLeft != 0 || localRight != 0) {
        val mySineValue = math.sin(note)
        left += mySineValue * localLeft
        right += mySineValue * localRight
      }
      note /= noteStep
      y += 1
    }
    target(0) = left / bacteriaPower / (width - 1)
    target(1) = right / bacteriaPower / (width - 1)
  }
}
