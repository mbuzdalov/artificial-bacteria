package alife

object Monsters:
  import Instruction._
  
  // first monster that works in the new implementation
  private val _1 = IArray(EnergyAt(0), Plus(0,1), Plus(2,2), EnergyAt(8), Divide(3,3), Log(5), Divide(1,6), Sin(7), Sin(7), MyWeight, MyWeight, Minus(10,5), EnergyAt(6), Divide(1,12), MyHealth)
  
  // apparently a very good and of minimal length
  private val _2 = IArray(EnergyAt(0), MyHealth, HealthAt(1), Log(3), Times(4,3))
  
  val TheChosenOne: IArray[Instruction] = _2
