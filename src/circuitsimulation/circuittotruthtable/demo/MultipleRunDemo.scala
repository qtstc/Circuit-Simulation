package circuitsimulation.circuittotruthtable.demo

import circuitsimulation.circuittotruthtable.circuit.elements._
import circuitsimulation.circuittotruthtable.analyzer._
import circuitsimulation.circuittotruthtable.circuit.Circuit
import circuitsimulation.circuittotruthtable.circuit.SimulantType

object MultipleRunDemo {
  def main(args: Array[String]) {

    val circuit1 = new Circuit() with CommonGates
    import circuit1._

    val in1 = new Wire("input1", SimulantType.INPUT_WIRE)
    val in2 = new Wire("input2", SimulantType.INPUT_WIRE)
    val in3 = new Wire("input3", SimulantType.INPUT_WIRE)
    val t1 = new Wire("track1", SimulantType.TRACKED_WIRE)
    val t2 = new Wire("track2", SimulantType.TRACKED_WIRE)
    val out1 = new Wire("output1", SimulantType.OUTPUT_WIRE)
    val out2 = new Wire("output2", SimulantType.OUTPUT_WIRE)

    xorGate(in1, in2, t1)
    norGate(in1, in2, t2)
    nandGate(t1, t2, out1)
    andGate(in2, in3, out2)

    //hasLoop()
    val analyzer1 = new MultipleRunAnalyzer(circuit1)
    analyzer1.startSimulation
    //analyzer1.getSimulationResult()
  }
}

