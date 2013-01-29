package circuitsimulation.circuittotruthtable.demo

import circuitsimulation.circuittotruthtable.circuit.elements._
import circuitsimulation.circuittotruthtable.analyzer._
import circuitsimulation.circuittotruthtable.circuit.Circuit
import circuitsimulation.circuittotruthtable.circuit.SimulantType

object SingleRunDemo {
  def main(args: Array[String]) {

    val circuit2 = new Circuit() with Adders
    import circuit2._

    val A0 = new Wire("A0", SimulantType.INPUT_WIRE, true)
    val B0 = new Wire("B0", SimulantType.INPUT_WIRE, false)
    val A1 = new Wire("A1", SimulantType.INPUT_WIRE, false)
    val B1 = new Wire("B1", SimulantType.INPUT_WIRE, true)
    val C0 = new Wire("C0", SimulantType.INPUT_WIRE, false)

    val C1 = new Wire("C1", SimulantType.TRACKED_WIRE)
    val C2 = new Wire("C2", SimulantType.OUTPUT_WIRE)
    val S0 = new Wire("S0", SimulantType.OUTPUT_WIRE)
    val S1 = new Wire("S1", SimulantType.OUTPUT_WIRE)

    fullAdder(A0, B0, C0, S0, C1)
    fullAdder(A1, B1, C1, S1, C2)

    //hasLoop()
    val analyzer2 = new SingleRunAnalyzer(circuit2)
    analyzer2.startSimulation
    //analyzer2.getSimulationResult()
  }
}

