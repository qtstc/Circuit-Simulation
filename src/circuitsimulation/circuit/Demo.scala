package circuitsimulation.circuit

import circuitsimulation.circuit.elements.CommonGates
import circuitsimulation.analyzer._

object Demo {
  def main(args: Array[String]) {
    
    val circuit = new Circuit() with CommonGates
    import circuit._
    
    val in1 = new Wire("input1",SimulantType.INPUT_WIRE)
    val in2 = new Wire("input2",SimulantType.INPUT_WIRE)
    val in3 = new Wire("input3",SimulantType.INPUT_WIRE)
    val t1 = new Wire("track1",SimulantType.TRACKED_WIRE)
    val t2 = new Wire("track2",SimulantType.TRACKED_WIRE)
    val out = new Wire("output",SimulantType.OUTPUT_WIRE)
    val out2 = new Wire("output2",SimulantType.OUTPUT_WIRE)
    
    xorGate(in1,in2,t1)
    orGate(in1,in2,t2)
    andGate(t1,t2,out)
    nandGate(out,in3,out2)
    
    
    val analyzer = new MultipleRunAnalyzer(circuit)
    analyzer.startSimulation
  }
}

