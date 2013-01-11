package circuitsimulation.circuit

import circuitsimulation.circuit.elements.CommonGates
import circuitsimulation.analyzer._

object Demo {
  def main(args: Array[String]) {
    
    val circuit = new Circuit() with CommonGates
    import circuit._
    
    val in1 = new Wire("Input1",SimulantType.INPUT_WIRE,true)
    val in2 = new Wire("Input2",SimulantType.INPUT_WIRE,true)
    val out1 = new Wire("Output1",SimulantType.OUTPUT_WIRE)
    val out2 = new Wire("Output2",SimulantType.OUTPUT_WIRE)
    
    xorGate(in1,in2,out1)
    orGate(in1,in2,out2)
    //circuit.start
    
    val analyzer = new SingleRunAnalyzer(circuit)
    analyzer.startSimulation
    
  }
}

