package alife

import alife.Action.{Eat, Fork, Move, RotateMinus, RotatePlus}
import alife.Mutation.Smooth

object Monsters:
  import Instruction._

  private val availableMonsters = Map[IndexedSeq[Action], IndexedSeq[IArray[Instruction]]](
    IndexedSeq(Fork(Smooth), RotateMinus, RotatePlus, Move, Eat) -> IndexedSeq(
      // of minimal length, works very quickly but is sometimes unreliable
      IArray(FoodAt(0), MyHealth, HealthAt(1), Log(3), Times(4,3)),
      // this did not give a chance to the previous monster
      IArray(MyHealth, Sin(1), FoodAt(0), DebrisAt(0), Sigmoid(0,1), HealthAt(8), Minus(3,5)),
      // this was an evolution of the previous one
      IArray(FoodAt(0), Plus(1,1), MyHealth, FoodAt(3), Sin(2), Times(3,5)),
    ),
    IndexedSeq(Eat, Move, Fork(Smooth), RotatePlus, RotateMinus) -> IndexedSeq(
      IArray(FoodAt(0), MyWeight, HealthAt(0), Const(0.2594246799612059), HealthAt(2), Log(3), Exp(3), Times(6,7)),
    ),
    IndexedSeq(RotateMinus, RotatePlus, Fork(Smooth), Move, Eat) -> IndexedSeq(
      IArray(DebrisAt(3), FoodAt(0), Divide(2,1), Const(0.050015250662791), DebrisAt(1), Sin(3)),
    )
  )
  
  def chooseFor(actionSequence: IArray[Action]): Option[IArray[Instruction]] =
    availableMonsters.get(actionSequence.toIndexedSeq).flatMap(_.lastOption)
