package circuitsimulation.circuittotruthtable.analyzer

import circuitsimulation.circuittotruthtable.circuit.ParallelSimulation._
import circuitsimulation.circuittotruthtable.circuit._
import scala.actors.Actor
import Actor._
import scala.collection.mutable.ListBuffer

/**
 * A sub class of Analyzer. 
 * It takes a fully constructed Circuit instance
 * and run simulations on it.
 * The number of simulations run is determined by the
 * input wires of the Circuit.
 * Basically the analyzer goes through all possible input combinations.
 */
class MultipleRunAnalyzer(circuit:Circuit) extends Analyzer(circuit) {
  
  //A class used group the data of a truth table column.
  //The first variable is the name of the column and the second is a list
  //of the cells in that column
  case class TruthTableColumn(columnName:String,cells:ListBuffer[Boolean])
  
  private val inputNumber = circuit.getInputNumber()//The number of inputs
  private val inputCombinations = getCombinations(inputNumber)
  
  //Used to keep track of the progress of simulation. Used as an index for inputCombinations
  private var counter = 0
  
  //Truth table for different types of wires.
  //The key of the map is the name of the wire.
  //The value of the map is the truth table column corresponds to the wire
  private var inputTruthTable = Map.empty[String,TruthTableColumn]
  private var trackedTruthTable = Map.empty[String,TruthTableColumn]
  private var outputTruthTable = Map.empty[String,TruthTableColumn]
  
  
  def startSimulation()
  {
    nextRun()
  }
  
  /**
   * First set the inputs of the circuit to a new set of values.
   * Then start the simulation.
   */
  private def nextRun()
  {
      circuit.setInputs(inputCombinations(counter))
      counter += 1
      circuit.start()
  }
  
  protected def reactToFinishedSimulation()
  {
    //If there are still more input combinations to be simulated
    if (counter != inputCombinations.length)
    {
      circuit.reset()
      nextRun()
    }
    else
    {
      println(truthTableToString())
      exit()
    }
  }

  protected def processLog(log: SimulationLog)
  {
    /**
     * Helper method used to process the WireLog for an finished run.
     * It basically adds the result of the run to the map.
     */
    def processWireLog(wl:List[WireLog],map:Map[String,TruthTableColumn]):Map[String,TruthTableColumn] =
    {
      var m = map
      for(w<-wl)
      {
        val value = w.wLog.last.value//Take the last value of the wire log ,which is the result of the simulation
        if(m.contains(w.wireName))
          m(w.wireName).cells.append(value)
        else
        {
          val buffer = ListBuffer.empty[Boolean]
          buffer.append(value)
          m += (w.wireName->new TruthTableColumn(w.wireName,buffer))
        }
      }
      m
    }
    
    inputTruthTable = processWireLog(log.inputLog, inputTruthTable)
    outputTruthTable = processWireLog(log.outputLog,outputTruthTable)
    trackedTruthTable = processWireLog(log.trackedLog,trackedTruthTable)
  }
  
  /**
   * Convert truth table data to string. 
   * Only to be called when all runs are finished.
   */
  private def truthTableToString() =
  {
    var text = ""
    
    /**
     * Helper method used to convert the truth table map to a list.
     * Necessary because we want the columns to be sorted according to
     * the name of the wires.
     */
    def mapToSortedList(m:Map[String,TruthTableColumn]) = m.toList.sortWith((e1,e2)=>(e1._1<e2._1))
    
    val inputList = mapToSortedList(inputTruthTable)
    val outputList = mapToSortedList(outputTruthTable)
    val trackedList = mapToSortedList(trackedTruthTable)
    
    //First add the names of the wires
    for(e<-inputList)
      text += "\t"+e._1
    for(e<-trackedList)
      text += "\t"+e._1
    for(e<-outputList)
      text += "\t"+e._1
    text += "\n"
    
    //Then print the data
    for(i<-0 until inputCombinations.length)
    {
      for(e<-inputList)
        text += "\t" + e._2.cells(i)
      for(e<-trackedList)
        text += "\t" + e._2.cells(i)
      for(e<-outputList)
        text += "\t" + e._2.cells(i)
      text += "\n"
    }
    text
  }
  
  /**
   * Get all combinations for a number of slots.
   * The length of the returned list is 2 to the power of slot number.
   * And each entry of the returned list is a list of boolean of length
   * equal to slot number.
   */
  private def getCombinations(slotNum:Int):List[List[Boolean]]= slotNum match
  {
    case 0 => List(List())
    case n => 
      val lastCombination = getCombinations(slotNum-1)
      val trueCom = lastCombination.map((s : List[Boolean])=>true::s)
      val falseCom = lastCombination.map((s : List[Boolean]) => false::s)
      falseCom ::: trueCom
  }
}