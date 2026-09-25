package alife.persistence

import alife.Config

trait Persistor extends AutoCloseable:
  def config: Config
  def isWritable: Boolean
  def hasMoreIterations: Boolean
  def startIteration(): Unit
  def finishIteration(): Unit
  def hasNextIndividualAction: Boolean
  def nextIndividualAction: Int
  def writeIndividualAction(action: Int): Unit
