package alife

case class StepStatistics(iteration: Long,
                          maxGenomeSize: Int, averageGenomeSize: Double, numberOfBacteria: Int,
                          averageHealth: Double, maximalHealth: Double, totalFood: Double, maxFood: Double,
                          actionCounts: IArray[Int],
                          maxLife: Int, maxChildren: Int, maxTravelDistance: Int, maxSpeed: Double, nMonsters: Int,
                          avgNecessaryInstructions: Double, avgNecessaryInstructionRatio: Double)
