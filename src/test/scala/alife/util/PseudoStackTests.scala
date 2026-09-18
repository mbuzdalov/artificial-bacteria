package alife.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PseudoStackTests extends AnyFlatSpec with should.Matchers:
  "An empty PseudoStack" should "behave correctly" in:
    val ps = PseudoStack()
    ps.size shouldBe 0
    ps(0) shouldBe 0.0
    ps(-1) shouldBe 0.0
    ps(1) shouldBe 0.0
    ps(Int.MinValue) shouldBe 0.0
    ps(Int.MaxValue) shouldBe 0.0
  
  "A PseudoStack with two elements" should "behave correctly" in:
    val ps = PseudoStack()
    ps.size shouldBe 0
    
    ps.push(239)
    ps.size shouldBe 1
    ps(0) shouldBe 0.0
    ps(-1) shouldBe 0.0
    ps(1) shouldBe 239.0
    ps(2) shouldBe 0.0
    
    ps.push(42)
    ps.size shouldBe 2
    ps(0) shouldBe 0.0
    ps(-1) shouldBe 0.0
    ps(1) shouldBe 42.0
    ps(2) shouldBe 239.0
    ps(3) shouldBe 0.0
    
    ps.clear()
    ps.size shouldBe 0
    ps(0) shouldBe 0.0
    ps(1) shouldBe 0.0
