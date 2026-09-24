package alife.persistence

import alife.{Config, Individual, Instruction}

trait PersistorFactory:
  def connect(config: Config): Persistor

object PersistorFactory:
  object Dummy extends PersistorFactory:
    override def connect(config: Config): Persistor =
      val alias = config
      new Persistor:
        private var _nCompletedIterations: Long = 0
        override def config: Config = alias
        override def nCompletedIterations: Long = _nCompletedIterations
        override def close(): Unit = ()
        
        override def connectToIteration(iterationNo: Long): Persistor.Iteration =
          if iterationNo < 0 then throw IllegalArgumentException("Negative iteration numbers are invalid")
          if iterationNo > _nCompletedIterations
          then throw IllegalArgumentException(s"Cannot connect to iteration $iterationNo because there are only $_nCompletedIterations completed iterations")
          new Persistor.Iteration:
            override def registerBirth(individual: Individual, parentID: Long): Unit = ()
            override def registerDeath(individual: Individual): Unit = ()
            override def startMonsterAction(x: Int, y: Int, genome: IArray[Instruction]): Unit = ()
            override def startEraseAction(x: Int, y: Int, radius: Int): Unit = ()
            override def startIncreaseFoodAction(x: Int, y: Int, radius: Int, foodAmount: Double): Unit = ()
            override def hasNextCachedAction: Boolean = false
            override def nextCachedAction: Int = throw IllegalStateException("No cached actions")
            override def canPerformInteractiveActions: Boolean = false
            override def writeAction(action: Int): Unit = ()
            override def close(): Unit = if iterationNo == _nCompletedIterations then _nCompletedIterations += 1
