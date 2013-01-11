/*
 * Copyright (C) 2007-2008 Artima, Inc. All rights reserved.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Example code from:
 *
 * Programming in Scala (First Edition, Version 6)
 * by Martin Odersky, Lex Spoon, Bill Venners
 *
 * http://booksites.artima.com/programming_in_scala
 */

package circuitsimulation.circuit

import scala.actors.Actor
import Actor._
import circuitsimulation.circuit.SimulantType._

object ParallelSimulation {
  
  //Message sent to each simulants at the end of each stage.
  case class Ping(time: Int)
  //Message sent back to the clock after the simulant has processed the Ping received.
  //It marks the end of that stage because the simulant must first process all previous
  //requests before reaching the Ping.
  case class Pong(time: Int, from: Actor)
  
  case object Start//Message used to sginal the start of the simulation
  case object Stop//Message used to signal the end of the simualation
  
  case object Finished//Message sent to analyzer to indicate the end of the simulation
  case object Finalized//Message sent to analyzer to indicate that the clock is about to exit()
  
  //Message used to add a task to the simulation.
  //It is always sent from the simulant to the clock
  //since the clock keeps track of all the tasks.
  case class AfterDelay(delay: Int, msg: Any, target: Actor)
  case class ClockLogEntry(msg:String, time:Int)//To be sent to the analyzer for logging 

  class Clock(finishAtTime:Int) extends Actor {
    
    //Disable the finish at a specific time feature.
    //The simulation will not stop until it ends itself.
    def this(){this(-1)}
    
    private var running = false
    private var currentTime = 0//Keeps track of the simulation time.
    
    //List of tasks. In a sequential circuit,
    //the list should be empty by the end of the simulation.
    private var agenda: List[WorkItem] = List()
    
    private var analyzer:Actor = null
    private var clockLog:List[ClockLogEntry] = List()
    
    //List of simulants.
    private var allSimulants: List[Simulant] = List()
    
    //List of simulants that have not responded to the clock's Ping in each stage.
    //At the beginning of each stage, all simulants are put into this list.
    //At the end of each stage, this list needs to be empty.
    private var busySimulants: Set[Actor] = Set.empty

    start()

    /**
     * Contains the task that is to be done by a simulant.
     * It is used in the task scheduling message sent from simulants 
     * to the clock.
     * The clock then send the msg(work to be done) to target
     * at time.
     */
    case class WorkItem(time: Int, msg: Any, target: Actor)

    /**
     * Insert a task to the list of tasks.
     * It always insert the task to the beginning of the list.
     */
    private def insert(ag: List[WorkItem],
      item: WorkItem): List[WorkItem] = {

      if (ag.isEmpty || item.time < ag.head.time) item :: ag
      else ag.head :: insert(ag.tail, item)
    }

    /**
     * Overrides the method from the Actor class
     */
    def act() {
      loop {
        //If it is running and all simulants have finished their task for the stage
        if (running && busySimulants.isEmpty)
          advance()

        //Only react to one message at a time 
        reactToOneMessage()
      }
    }

    /**
     * This method checkes whether the simulation has ended.
     */
    private def isSimulationFinished:(Boolean,String) = finishAtTime match
    {
      //If the user did not specify a time at which the simulation needs to end,
      //pnly end when the list of tasks is empty.
      case -1 =>
        if (agenda.isEmpty && currentTime > 0) 
        	(true,"** Agenda empty.  Clock exiting at time "+currentTime + ".")
        else 
        (false,"")
      //If the user specified a time at which the simulation needs to end,
      //end the simulation when reach that time.
      case n if n>0 => 
        if(n == currentTime)
          (true,"Clock exiting at time "+currentTime + " as requested.")
        else if(n < currentTime)
          (true,"Error, clock should have exited at "+ n+".")
        else 
          (false,"");
      //Throw an exception otherwise.
      case n =>
        throw new IllegalArgumentException("Exiting time "+n+" needs to be a positive integer!")
    }
    
    /**
     * Advance to the next stage.
     */
    private def advance() {
      //First check whether the simulation has finished.
      val finished = isSimulationFinished;
      if(finished._1)
      {
        log(finished._2)
        if(analyzer != null)
           analyzer ! Finished
        else
          self ! Stop
        return
      }
      
      //Increase current time maintained by the clock by one.
      currentTime += 1
      log("Advancing to time " + currentTime)

      //Send the tasks to be completed at the current stage to the simulants
      processCurrentEvents()
      //Ping the simulants afterwards. 
      //This way the simulants only see the ping after finishing processing the tasks sent from the clock.
      for (sim <- allSimulants)
        sim ! Ping(currentTime)

      //Add all simulants to the list of busy simulants
      //since all of them were sent zero or more tasks and a Ping.
      busySimulants = Set.empty ++ allSimulants
    }

    /**
     * Send the tasks scheduled to be done at currentTime
     * to the simulants that should complete the tasks.
     */
    private def processCurrentEvents() {
      //Only take those tasks that need to be finished at this stage.
      val todoNow = agenda.takeWhile(_.time <= currentTime)

      //Remove them from the list of tasks.
      agenda = agenda.drop(todoNow.length)

      //Send them to the simulants
      for (WorkItem(time, msg, target) <- todoNow) {
        assert(time == currentTime)
        target ! (msg,time)
      }
    }

    /**
     * React to a single message.
     */
    private def reactToOneMessage() {
      react {
        //A message send from a simulant to the clock.
        //Used to add msg(a task) to the list of tasks maintained by the clock
        case AfterDelay(delay, msg, target) =>
          val item = WorkItem(currentTime + delay, msg, target)
          agenda = insert(agenda, item)

        //Pong sent from simulants, indicating that simulant
        //is done with the tasks of that stage.
        case Pong(time, sim) =>
          assert(time == currentTime)
          assert(busySimulants contains sim)
          busySimulants -= sim//Remove the simulant from the busySimulants list

        //Message that starts the simulation
        case Start => running = true

        //Message that stops the simulation
        case Stop =>
          //First stop all simulants
          for (sim <- allSimulants)
            sim ! Stop
          //Then send a message to analyzer indicating
          //current simulation is done.
          //This message is supposed to be sent after calling exit()
          //because the analyzer might need to call restart()
          //on this Clock instance to start a new simulation. 
          //And restart() throws an exception
          //if the Clock does not call exit()(become terminated) first.
          //However, because it is impossible to send any messages after calling
          //exit(), we have to send out the finish message before calling exit()
          if(analyzer != null)
          {
        	  analyzer ! Finalized
          }
          //No computing intensive task should be put here.
          //Otherwise the analyzer, which runs on another thread,
          //might call restart() on this clock instance before exit()
          //is called.
          //In the current implementation, the analyzer first check
          //whether the clock is terminated before calling restart().
          //If the clock is not terminated, it just keep checking
          //until it is terminated.
          exit()
      }
    }
    
    /**
     * Log a message.
     */
    private def log(msg:String)
    {
      //If no analyzer is attached, simply print the message.
      if(analyzer == null)
        println(msg)
      //Otherwise, save the message first.
      else
        clockLog = ClockLogEntry(msg,currentTime)::clockLog
    }
    
      /**
     * Add a simulant to the list tracked by the clock.
     */
    def add(sim: Simulant) {
      allSimulants = sim :: allSimulants
    }
    
        
    /**
     * Set the analyzer of this simulation.
     * An analyzer is used to run a simulation multiple times
     * and analyze the result.
     */
    def setAnalyzer(analyzer:Actor)
    {
       this.analyzer = analyzer
    }
    
   /**
    * Reset the simulation.
    * Empty the task list and set the current time to 0 again.
    * Also call reset() and restart on all simulants.
    * Only call this method when the simulation already exited
    */
    def reset()
    {
      clockLog = List()
      running = false
      currentTime = 0
      busySimulants = Set.empty
      agenda = List()
      restart()
      for(simulant<-allSimulants)
      {
        simulant.restart()
        simulant.reset()
      }
    }
    
    /**
     * Return all simulants whose type meets certain criteria 
     */
    def getSimulantsWithType(f:(SimulantType => Boolean)):List[Simulant] =
    {
      for(simulant<-allSimulants;if(f(simulant.simulantType)))yield simulant
    }
    
    /**
     * Get the log of the clock. Only to be called
     * after the simulation is finished.
     */
    def getClockLog() = clockLog.reverse
  }

  /**
   * Super class inherited by all circuit components.
   * All simulants maintain a instance of clock which
   * the simulant used communicates with.
   */
  trait Simulant extends Actor {
    def simulantType:SimulantType;
    protected val clock: Clock
    protected def handleSimMessage(msg: Any,time:Int)
    protected def simStarting() {}
    def reset(){}
    def act() {
      loop {
        react {
          case Stop => exit()
          case Ping(time) =>
            if (time == 1) simStarting()
            clock ! Pong(time, self)
          case (msg,time:Int) => handleSimMessage(msg,time)
        }
      }
    }
    start()
  }
}
