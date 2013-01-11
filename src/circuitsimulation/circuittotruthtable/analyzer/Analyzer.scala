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
abstract class Analyzer(circuit:Circuit) extends Actor{
  
  circuit.setAnalyzer(this)
  start()
  
  protected def reactToFinishedSimulation()
  protected def processLog(log:SimulationLog)  
  def startSimulation()
  
  def act()
  {
    loop
    {
      react
      {
        case Finished =>
          processLog(circuit.getLog())
          circuit.stop()
        case Finalized =>
          //Check back if the previous simulation is not done yet
          if(circuit.getClockState != Actor.State.Terminated)
            self ! Finalized
          reactToFinishedSimulation()
      }
    }
  }
}