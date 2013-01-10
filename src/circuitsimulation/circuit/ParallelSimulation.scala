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

object ParallelSimulation {
  
  //Message sent to each simulants at the end of each stage.
  case class Ping(time: Int)
  //Message sent back to the clock after the simulant has processed the Ping received.
  //It marks the end of that stage because the simulant must first process all previous
  //requests before reaching the Ping.
  case class Pong(time: Int, from: Actor)
  
  case object Start//Message used to sginal the start of the simulation
  case object Stop//Message used to signal the end of the simualation
  
  //Message used to add a task to the simulation.
  //It is always sent from the simulant to the clock
  //since the clock keeps track of all the tasks.
  case class AfterDelay(delay: Int, msg: Any, target: Actor)

  class Clock(finishAtTime:Int) extends Actor {
    
    //Disable the finish at a specific time feature.
    //The simulation will not stop until it ends itself.
    def this(){this(-1)}
    
    private var running = false
    private var currentTime = 0//Keeps track of the simulation time.
    
    //List of tasks. In a sequential circuit,
    //the list should be empty by the end of the simulation.
    private var agenda: List[WorkItem] = List()
    
    //List of simulants.
    private var allSimulants: List[Actor] = List()
    
    //List of simulants that have not responded to the clock's Ping in each stage.
    //At the beginning of each stage, all simulants are put into this list.
    //At the end of each stage, this list needs to be empty.
    private var busySimulants: Set[Actor] = Set.empty

    start()

    /**
     * Add a simulant to the list tracked by the clock.
     */
    def add(sim: Simulant) {
      allSimulants = sim :: allSimulants
    }

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
    def isSimulationFinished:(Boolean,String) = finishAtTime match
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
    def advance() {
      //First check whether the simulation has finished.
      val finished = isSimulationFinished;
      if(finished._1)
      {
        println(finished._2)
        self ! Stop
        return
      }
      
      //Increase current time maintained by the clock by one.
      currentTime += 1
      println("Advancing to time " + currentTime)

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
        target ! msg
      }
    }

    /**
     * React to a single message.
     */
    def reactToOneMessage() {
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
          for (sim <- allSimulants)
            sim ! Stop
          exit()
      }
    }
  }

  /**
   * Super class inherited by all circuit components.
   * All simulants mantain a instance of clock which
   * the simulant used communicates with.
   */
  trait Simulant extends Actor {
    val clock: Clock
    def handleSimMessage(msg: Any)
    def simStarting() {}
    def act() {
      loop {
        react {
          case Stop => exit()
          case Ping(time) =>
            if (time == 1) simStarting()
            clock ! Pong(time, self)
          case msg => handleSimMessage(msg)
        }
      }
    }
    start()
  }
}
