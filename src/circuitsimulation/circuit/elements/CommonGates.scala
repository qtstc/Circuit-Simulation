package circuitsimulation.circuit.elements

import circuitsimulation.circuit.Circuit

trait CommonGates extends Circuit {
  
  /**
   * AND gate is built by connecting an inverter to the output of a NAND gate.
   */
  def andGate(inputOne: Wire, inputTwo: Wire, output: Wire)
  {
    val nandGateOutput = new Wire
    nandGate(inputOne, inputTwo, nandGateOutput)
    inverter(nandGateOutput, output)
  }
  
  /**
   * OR gate is built by connecting an inverter to the output of a NOR gate.
   */
  def orGate(inputOne: Wire, inputTwo: Wire, output: Wire)
  {
    val norGateOutput = new Wire
    norGate(inputOne, inputTwo, norGateOutput)
    inverter(norGateOutput, output)
  }
  
  /**
   * XOR gate is built from the equation A * ~B + ~A * B
   */
  def xorGate(inputOne:Wire, inputTwo: Wire, output:Wire)
  {
    val notInputOne = new Wire
    inverter(inputOne, notInputOne)
    val notInputTwo = new Wire
    inverter(inputTwo, notInputTwo)
    
    val andLeft = new Wire
    andGate(inputOne, notInputTwo, andLeft)
    val andRight = new Wire
    andGate(notInputOne,inputTwo,andRight)
    
    orGate(andLeft,andRight,output)
  }
  
  /**
   * XNOR gate is built from the equation A * B + ~A * ~B
   */
  def xnorGate(inputOne:Wire, inputTwo: Wire, output:Wire)
  {
    val notInputOne = new Wire
    inverter(inputOne, notInputOne)
    val notInputTwo = new Wire
    inverter(inputTwo, notInputTwo)
    
    val andLeft = new Wire
    andGate(inputOne,inputTwo,andLeft)
    val andRight = new Wire
    andGate(notInputOne,notInputTwo,andRight)
    
    orGate(andLeft,andRight,output)
  }
}