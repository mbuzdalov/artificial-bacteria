package alife

object Monsters {
  import Instruction._

  val _1: IndexedSeq[Instruction] = Vector(Cos(-1), Exp(-1), MyHealth, Const(0.09507841871622413),
    Const(0.049785117511532584), HealthAt(8), Plus(3,0), HealthAt(3), Exp(7), Log(5), EnergyAt(7), HealthAt(5),
    Minus(5,10), Log(6), Times(0,-1), Minus(13,7), HealthAt(0), MyWeight, Sin(8), MyHealth, Const(0.5324097910260434),
    HealthAt(3), Times(11,7), Cos(8), MyHealth, Times(4,2), Const(0.267066143525828), Divide(5,6), Minus(13,13),
    Sin(10), MyHealth, Minus(23,21), Times(3,13), Plus(4,19), Sin(17), Exp(12), Exp(13), EnergyAt(8), Times(20,20),
    MyHealth, Const(0.3879856095322598), Plus(11,13), HealthAt(1), Log(13), Divide(7,15), Sin(27),
    Const(0.8423083481778832), Sin(5), Plus(41,28), DebrisAt(5), EnergyAt(1), Sin(8), Divide(20,31), Exp(1),
    HealthAt(4), Exp(15))

  val First: IndexedSeq[Instruction] = Vector(Sin(-1), Const(0.1407119580029622), Const(0.10100863812199412), MyHealth, Plus(1,1), Cos(1), Divide(5,1), Exp(-1), Plus(7,-1), Times(0,-1), HealthAt(6), HealthAt(2), Exp(2), Divide(11,11), MyWeight, HealthAt(6), MyWeight, EnergyAt(0), Cos(15), Minus(12,10), Times(8,13), MyHealth, DebrisAt(3), Sin(0), HealthAt(6), Log(21), HealthAt(2), Sigmoid(25,10), Sin(3), HealthAt(1), Times(0,8), MyHealth)
}
