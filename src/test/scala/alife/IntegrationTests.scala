package alife

import alife.persistence.SingleSourcePersistor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.zip.GZIPInputStream
import scala.util.Using

class IntegrationTests extends AnyFlatSpec with should.Matchers:
  private val test1Bytes =
    Using.resource(this.getClass.getResourceAsStream("test1.alf.gz")): res =>
      Using.resource(GZIPInputStream(res)): gz =>
        gz.readAllBytes()

  "Test persistent run" should "run OK in validation mode" in:
    val persistor = SingleSourcePersistor.forByteArray(test1Bytes, forValidation = true)
    val simulation = Simulation(persistor)
    while persistor.hasMoreIterations do
      simulation.simulationStep()
   
  it should "run OK in read mode" in:
    val persistor = SingleSourcePersistor.forByteArray(test1Bytes, forValidation = false)
    val simulation = Simulation(persistor)
    while persistor.hasMoreIterations do
      simulation.simulationStep()
