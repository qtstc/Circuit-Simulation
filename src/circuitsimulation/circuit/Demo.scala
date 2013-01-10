package circuitsimulation.circuit

import circuitsimulation.circuit.elements.CommonGates

object Demo {
  def main(args: Array[String]) {
    val circuit = new Circuit() with CommonGates
    import circuit._
    
    val in1 = new Wire("InputOne",true)
    val in2 = new Wire("Output")
    
    probe(in1)
    probe(in2)
    inverter(in1,in2)
    circuit.start()
    for(i <- 4 to 1000000000)
      i*(i-1)/(i-2)
    println("")
    println("")
    println("")
    circuit.reset
    in1.set(true);
    circuit.start
    
  }
}

