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
import ParallelSimulation._
import circuitsimulation.circuit.SimulantType._;

case class WireLogEntry(value:Boolean,time:Int)
case class WireLog(wireName:String,wLog:List[WireLogEntry])
case class SimulationLog(inputLog:List[WireLog],outputLog:List[WireLog],trackedLog:List[WireLog],clockLog:List[ClockLogEntry])

class Circuit(finishSimulation:Int) {
  def this(){this(-1)}
  
  private val clock = new Clock(finishSimulation)
  
  //Tasks to be done by a circuit element.
  case class SetSignal(sig: Boolean)
  case class SignalChanged(wire: Wire, sig: Boolean)

  val WireDelay = 1
  val InverterDelay = 2
  val NorGateDelay = 3
  val NandGateDelay = 3

  class Wire(name: String,wType: SimulantType, init: Boolean) extends Simulant {
    
    def this(name:String, wType: SimulantType){this(name,wType,false)}
    def this(name: String) { this(name,SimulantType.IGNORED_WIRE) }
    def this() { this("unnamed") }
    
    private var wireLog:List[WireLogEntry] = List()
    protected val clock = Circuit.this.clock
    clock.add(this)

    var sigVal = init
    private var observers: List[Actor] = List()
    
    protected def handleSimMessage(msg: Any,time:Int) 
    {
      msg match {
        case SetSignal(s) =>
          if (s != sigVal) {
            sigVal = s
            //Log the current wire state if the wire is not ignored
            if(isTracked(wType))
              log(time)
            signalObservers()
          }
      }
    }
    
    private def signalObservers() 
    {
      for (obs <- observers)
      {
        clock ! AfterDelay(
          WireDelay,
          SignalChanged(this, sigVal),
          obs)
      }
    }
    
    private def log(time:Int)
    {
      wireLog = WireLogEntry(sigVal,time)::wireLog
    }
    
    
    def simulantType = wType
    def set(sig:Boolean){sigVal = sig}
    override def reset() {sigVal = false;wireLog = List()}
    override def simStarting() { signalObservers();log(WireDelay)}
    def addObserver(obs: Actor) {
      observers = obs :: observers
    }
    override def toString = "Wire(" + name + ")"
    def getWireLog() = WireLog(name,wireLog.reverse)
  }

  private object DummyWire extends Wire("dummy")

  abstract class Gate(in1: Wire, in2: Wire, out: Wire)
    extends Simulant {
    protected def computeOutput(s1: Boolean, s2: Boolean): Boolean
    protected val delay: Int
    protected val clock = Circuit.this.clock
    
    clock.add(this)
    in1.addObserver(this)
    in2.addObserver(this)
    
    protected var s1 = in1.sigVal
    protected var s2 = in2.sigVal
    protected def handleSimMessage(msg: Any,time:Int) {
      msg match {
        case SignalChanged(w, sig) =>
          if (w == in1)
            s1 = sig
          if (w == in2)
            s2 = sig
          clock ! AfterDelay(delay,
            SetSignal(computeOutput(s1, s2)),
            out)
      }
    }
    
    def simulantType = SimulantType.NOT_A_WIRE;
    override def reset(){s1 = false;s2 = false}
  }
  
  def norGate(in1: Wire, in2: Wire, output: Wire) =
    new Gate(in1, in2, output) {
      protected val delay = NorGateDelay
      protected def computeOutput(s1: Boolean, s2: Boolean) = !(s1 || s2)
    }

  def nandGate(in1: Wire, in2: Wire, output: Wire) =
    new Gate(in1, in2, output) {
      protected val delay = NandGateDelay
      protected def computeOutput(s1: Boolean, s2: Boolean) = !(s1 && s2)
    }

  def inverter(input: Wire, output: Wire) =
    new Gate(input, DummyWire, output) {
      protected val delay = InverterDelay
      protected def computeOutput(s1: Boolean, ignored: Boolean) = !s1
    }

  /*
  /**
   * Probe class is used to make users aware of the state 
   * of a Wire instance. It is created as a class because 
   * we sometimes need to check whether a simulant instance
   * is a valid circuit element or only a probe.
   */
  protected class Probe(wire: Wire) extends Simulant
  {
    val clock = Circuit.this.clock
    clock.add(this)
    wire.addObserver(this)
    def handleSimMessage(msg: Any,time:Int) {
      msg match {
        case SignalChanged(w, s) =>
          println("signal " + w + " changed to " + s)
      }
    }
  }
  
  def probe(wire: Wire) = new Probe(wire)
  */
  
    /**
   * Reset the circuit.
   * Reset all components in the circuit to false
   * and remove all tracking histories.
   * Need to call set() on each input wire to change
   * the inputs before calling start() to run the simulation
   * again.
   * Only call this method when the simulation is finished.
   */
  def reset(){clock.reset();}
  
  /**
   * Set the analyzer for this circuit.
   * An analyzer is used to run and analyze the circuit.
   * Use null if no analyzer is needed.
   */
  def setAnalyzer(analyzer:Actor)
  {
    clock.setAnalyzer(analyzer)
  }
  
  /**
   * Get the log of this simulation.
   * To be called after the simulation is finished.
   */
  def getLog() =
  {
    val wires = clock.getSimulantsWithType(SimulantType.isTracked)
    var inputLog = List[WireLog]();
    var outputLog = List[WireLog]();
    var trackedLog = List[WireLog]();
    for(w<-wires)
    {
     val wire = w.asInstanceOf[Wire] 
     wire.simulantType match
     {
       case SimulantType.INPUT_WIRE =>
         inputLog = wire.getWireLog()::inputLog
       case SimulantType.OUTPUT_WIRE =>
         outputLog = wire.getWireLog::outputLog
       case SimulantType.TRACKED_WIRE =>
         trackedLog = wire.getWireLog::trackedLog
     }
    }
    SimulationLog(inputLog,outputLog,trackedLog,clock.getClockLog())
  }
  
  /**
   * Get the state of the clock.
   * Used to hide the implementation of clock
   * from users of this class.
   */
  def getClockState() = clock.getState
  
  def stop(){clock ! Stop}
  
  def start() { clock ! Start }
}
