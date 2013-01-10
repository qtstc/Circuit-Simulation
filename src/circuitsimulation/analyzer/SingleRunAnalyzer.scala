package circuitsimulation.analyzer

import circuitsimulation.circuit.ParallelSimulation._
import circuitsimulation.circuit._
import scala.actors.Actor
import Actor._

class SingleRunAnalyzer(circuit:Circuit) extends Analyzer(circuit) {
  
  private var logHistory:List[Log] = List()
  
  protected def reactToFinishedSimulation
  {
    printLog()
    exit()
  }
  
  protected def log(l:Log)
  {
    logHistory = l::logHistory
  }
  
  def printLog()
  {
      val reverseList = logHistory.reverse
      for(log <- reverseList)
        println(log.msg)
  }
}