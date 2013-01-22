package circuitsimulation.circuittotruthtable.analyzer

import circuitsimulation.circuittotruthtable.circuit.ParallelSimulation._
import circuitsimulation.circuittotruthtable.circuit._
import scala.actors.Actor
import Actor._
import circuitsimulation.circuittotruthtable.circuit._

/**
 * A class for analyzing instances of the Circuit class.
 * The analysis depends on the implementation of sub classes.
 *
 * If the Circuit instance passed to the constructor of this
 * class is modified after an instance of this class is
 * initialized, the behavior of that instance is undefined.
 *
 * Also, creating two Analyzers with the same Circuit instance
 * will cause an error.
 */
abstract class Analyzer(circuit: Circuit) extends Actor {

  circuit.setAnalyzer(this)
  start()

  /**
   * Method called after a simulation is finalized.
   * It may restart a new simulation depending on
   * the implementation of the child classes.
   */
  protected def reactToFinalizedSimulation()

  /**
   * Method called after a simulation is finished.
   * It processes the SimulationLog instance
   * passed from the circuit.
   */
  protected def processLog(log: SimulationLog)

  /**
   * Method used to start a simulation.
   * Implementation depends on child class.
   */
  def startSimulation()

  def act() {
    loop {
      react {
        case Finished =>
          processLog(circuit.getLog())
          circuit.stop()
        case Finalized =>
          //Check back if the previous simulation is not done yet
          if (circuit.getClockState != Actor.State.Terminated)
            self ! Finalized
          reactToFinalizedSimulation()
      }
    }
  }

  /**
   * Get the result of the simulation as a String.
   * Only call this after the simulation is done.
   * This method is designed to be used in the REPL,
   * when the result is not printed automatically after
   * the simulation is done.
   */
  def getSimulationResult(): String =
    {
      if (circuit.getClockState != Actor.State.Terminated)
        "Error: simulation is not finished yet."
      else
        simulationResult
    }

  /**
   * Result of the simulation as String.
   */
  protected def simulationResult: String
}