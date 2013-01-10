package circuitsimulation.circuit

import circuitsimulation.circuit.elements.CommonGates

object Demo {
  def main(args: Array[String]) {
    val circuit = new Circuit(1000) with CommonGates
    import circuit._
    
    val in1 = new Wire("InputOne",true)
    val in2 = new Wire("InputTwo",true)
    
    probe(in1)
    probe(in2)
    
    inverter(in1,in2)
    inverter(in2,in1)
    
    circuit.start()
  }
}

