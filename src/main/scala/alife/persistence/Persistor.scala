package alife.persistence

import alife.{Config, Individual, Instruction}

trait Persistor extends AutoCloseable:
  def config: Config
  def nCompletedIterations: Long
  def connectToIteration(iterationNo: Long): Persistor.Iteration

object Persistor:
  trait Iteration extends AutoCloseable:
    def registerBirth(individual: Individual, parentID: Long): Unit
    def registerDeath(individual: Individual): Unit
    def startMonsterAction(x: Int, y: Int, genome: IArray[Instruction]): Unit
    def startEraseAction(x: Int, y: Int, radius: Int): Unit
    def startIncreaseFoodAction(x: Int, y: Int, radius: Int, foodAmount: Double): Unit
    def canPerformInteractiveActions: Boolean
    def hasNextCachedAction: Boolean
    def nextCachedAction: Int
    def writeAction(action: Int): Unit
