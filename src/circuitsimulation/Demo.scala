package circuitsimulation

import circuitsimulation.circuitelement._

object Demo {
  def main(args: Array[String]) {
    val circuit = new Circuit with CommonGates
    import circuit._
    
    val in1 = new Wire("InputOne",true)
    val in2 = new Wire("InputTwo",true)
    
    probe(in1)
    probe(in2)
    
    
    circuit.start()
  }
}

