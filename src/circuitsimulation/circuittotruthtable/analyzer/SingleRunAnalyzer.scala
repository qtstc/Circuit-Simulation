package circuitsimulation.circuittotruthtable.analyzer

import circuitsimulation.circuittotruthtable.circuit.ParallelSimulation._
import circuitsimulation.circuittotruthtable.circuit._
import scala.actors.Actor
import Actor._

/**
 * A sub class of Analyzer that runs simulation on
 * a circuit instance only once.
 * This analyzer keeps track of the signal of each
 * tracked components of the circuit at every stage.
 */
class SingleRunAnalyzer(circuit: Circuit) extends Analyzer(circuit) {

  private var singleResult: String = "Error:simulation is not finished yet."

  protected def reactToFinalizedSimulation() = {
    println(simulationResult)
    exit()
  }

  protected def simulationResult = singleResult

  def startSimulation() {
    circuit.start()
  }

  protected def processLog(log: SimulationLog) {
    val endTime = log.clockLog.last.time

    singleResult = "Inputs:\n"
    for (w <- log.inputLog)
      singleResult += wireLogToString(w, endTime) + "\n"

    singleResult += "Tracked:\n"
    for (w <- log.trackedLog)
      singleResult += wireLogToString(w, endTime) + "\n"

    singleResult += "Outputs:\n"
    for (w <- log.outputLog)
      singleResult += wireLogToString(w, endTime) + "\n"
  }

  /**
   * Convert a single WireLog instance to one line of text.
   */
  private def wireLogToString(log: WireLog, endTime: Int) =
    {
      var result = log.wireName
      var currentList = log.wLog
      var lastSig = false
      for (time <- 1 to endTime) {
        val parts = currentList.span((entry: WireLogEntry) => entry.time == time)
        if (parts._1.length > 0) {
          lastSig = parts._1.last.value
          currentList = parts._2
        }
        result += "\t" + lastSig
      }
      result
    }
}