package alife.sound

import alife.Field

trait SoundSynthesizer {
  def synthesizeOneFrame(field: Field, time: Double, target: Array[Double]): Unit
}
