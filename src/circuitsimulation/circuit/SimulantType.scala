package circuitsimulation.circuit

/**
 * Object used to store the type of different simulant.
 * Used this class because all simulants are stored in a list,
 * and with type erasure, we need to keep track of the
 * type of a particular simulant.
 */
object SimulantType extends Enumeration {
  type SimulantType = Value
  val INPUT_WIRE, OUTPUT_WIRE, TRACKED_WIRE, IGNORED_WIRE,NOT_A_WIRE  = Value
  
  /**
   * Defines whether a simulant of certain type should be tracked.
   */
  def isTracked(sType:SimulantType):Boolean = sType match {
  case INPUT_WIRE => true
  case OUTPUT_WIRE => true
  case TRACKED_WIRE => true
  case IGNORED_WIRE => false
  case NOT_A_WIRE => false
}
}