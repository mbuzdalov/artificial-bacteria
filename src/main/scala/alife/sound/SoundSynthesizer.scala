package alife.sound

import alife.Field

/**
 * This encapsulates sound creation from the field and timestamp.
 * Because of pseudo-realtime requirements, we need to synthesize one sound frame at a time.
 */
trait SoundSynthesizer:
  /**
   * Synthesizes one sound frame based on the current `field` contents and current timestamp.
   * The results are to be written to `target`, which is an array whose dimension defines the number of channels
   * to generate for.
   * 
   * @param field the field to use
   * @param time the wall-clock timestamp
   * @param target the array to write frame to.
   */
  def synthesizeOneFrame(field: Field, time: Double, target: Array[Double]): Unit
