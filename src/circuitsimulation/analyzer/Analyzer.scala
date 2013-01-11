package circuitsimulation.analyzer

import circuitsimulation.circuit.ParallelSimulation._
import circuitsimulation.circuit._
import scala.actors.Actor
import Actor._
import circuitsimulation.circuit._

abstract class Analyzer(circuit:Circuit) extends Actor{
  
  var count:Int = 0
  circuit.setAnalyzer(this)
  start()
  
  protected def reactToFinishedSimulation()
  protected def processLog(log:SimulationLog)  
  
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
  
  def startSimulation()
  {
    circuit.start()
  }
}