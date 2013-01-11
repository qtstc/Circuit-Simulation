package circuitsimulation.analyzer

import circuitsimulation.circuit.ParallelSimulation._
import circuitsimulation.circuit._
import scala.actors.Actor
import Actor._

class SingleRunAnalyzer(circuit:Circuit) extends Analyzer(circuit) {
  
  protected def reactToFinishedSimulation
  {
    exit()
  }
  
  protected def processLog(log:SimulationLog)  
  {
    val endTime = log.clockLog.last.time
    
    println("Inputs:")
    for(w <- log.inputLog)
      println(wireLogToString(w,endTime))
    
    println("Outputs:")
    for(w <-log.outputLog)
      println(wireLogToString(w, endTime))
  }
  
  private def wireLogToString(log:WireLog,endTime:Int) =
  {
    var result = log.wireName
    var currentList = log.wLog
    var lastSig = false
    for(time<-1 to endTime)
    {
      val parts = currentList.span((entry:WireLogEntry) => entry.time == time)
      if(parts._1.length > 0)
      {
        lastSig = parts._1.last.value
        currentList = parts._2
      }
      result += "\t"+lastSig
    }
    result
  }
}