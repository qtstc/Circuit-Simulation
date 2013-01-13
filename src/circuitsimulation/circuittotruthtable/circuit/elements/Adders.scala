package circuitsimulation.circuittotruthtable.circuit.elements

import circuitsimulation.circuittotruthtable.circuit.Circuit

trait Adders extends Circuit with CommonGates {

  /**
   * Half adder consists of an XOR gate and an AND gate.
   * The output of the XOR gate is the sum.
   * And the output of the AND gate is the carry.
   */
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) {
    xorGate(a, b, s)
    andGate(a, b, c)
  }

  /**
   * A full adder is built with two OR gates.
   */
  def fullAdder(a: Wire, b: Wire, cin: Wire,
    sum: Wire, cout: Wire) {

    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }
}
