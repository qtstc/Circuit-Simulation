package circuitsimulation.circuit

object WireType extends Enumeration {
  type WireType = Value
  val INPUT, OUPUT, TRACKED, IGNORED = Value
}