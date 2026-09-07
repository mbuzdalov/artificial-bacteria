package alife.sound

import alife.Field
import alife.util.Loops

/**
 * This is the current default sound synthesis implementation,
 * which assigns the frequency to the vertical coordinate and generates sound at this frequency.
 *
 * Their strength and panoramic position depends on the distribution
 * of bacteria (and their healths) at this vertical coordinate.
 * This creates a sound which is panned according to the density of bacteria
 * health on the field.
 *
 * This implementation has the runtime per frame that is proportional to the field's height,
 * so the performance is reasonable.
 */
object DefaultSynthesizer extends SoundSynthesizer:
  /**
   * @inheritdoc
   */
  override def synthesizeOneFrame(field: Field, time: Double, target: Array[Double]): Unit =
    // we want =8 octaves, each taking a multiple of 12 pixels = a multiple of 96
    val baseFrequency = 55 * math.Pi * 2 // 55 Hz = 440 / 8 corresponds to the lowest row of pixels
    val height = field.height
    val pixelsPerOctave: Double = height / 8.0
    val outputScale = 100.0 / (field.width - 1)
    
    var left, right = 0.0
    var note = math.pow(2, (height - 1) / pixelsPerOctave) * baseFrequency * time
    val noteStep = math.pow(2, 1 / pixelsPerOctave)
    Loops.foreach(0, height): y =>
      // Getting amplitudes from the field.
      // A very left individual contributes a lot to distances from right.
      // This happens asynchronously with whatever is happening in the field,
      // but for the sake of entertainment small data inconsistencies do not matter.
      val localLeft = field.getSumOfDistancesFromRight(y)
      val localRight = field.getSumOfDistancesFromLeft(y)
      if localLeft != 0 || localRight != 0 then
        // Only run heavy math when needed
        val mySineValue = math.sin(note)
        left += mySineValue * localLeft
        right += mySineValue * localRight
      end if
      note /= noteStep
    
    target(0) = left * outputScale
    target(1) = right * outputScale
