package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "in1=false in2=false")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "in1=true in2=false")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "in1=false in2=true")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "in1=true in2=true")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "in1=false in2=false")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "in1=true in2=false")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "in1=false in2=true")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "in1=true in2=true")
  }

  test("demux example") {
    val in, c0, c1, c2 = new Wire
    val o0, o1, o2, o3, o4, o5, o6, o7 = new Wire
    val c = List(c2, c1, c0)
    val out = List(o7, o6, o5, o4, o3, o2, o1, o0) 
    in.setSignal(true)
    c2.setSignal(true)
    c1.setSignal(false)
    c0.setSignal(true)
    demux(in, c, out)
    assert(out(0).getSignal == false, "o7=false")
    assert(out(1).getSignal == false, "o6=false")
    assert(out(2).getSignal == true, "o5=true")
    assert(out(3).getSignal == false, "o4=false")
    assert(out(4).getSignal == false, "o3=false")
    assert(out(5).getSignal == false, "o2=false")
    assert(out(6).getSignal == false, "o1=false")
    assert(out(7).getSignal == false, "o0=false")
  }
}
