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

  class Wire(name: String, init: Boolean) extends Simulant {
    def this(name: String) { this(name, false) }
    def this() { this("unnamed") }

    val clock = Circuit.this.clock
    clock.add(this)

    private var sigVal = init
    private var observers: List[Actor] = List()
    def handleSimMessage(msg: Any,time:Int) {
      msg match {
        case SetSignal(s) =>
          //If we remove the check here,
          //the simulation will run less efficiently.
          //However, this check is also causing some
          //uncertainties in the result because
          //if the value of a wire is changed multiple
          //times in one stage, the sequence at which 
          //the change happens is not guaranteed
          if (s != sigVal) {
            sigVal = s
            signalObservers()
          }
      }
    }

    def signalObservers() {
      for (obs <- observers)
      {
        clock ! AfterDelay(
          WireDelay,
          SignalChanged(this, sigVal),
          obs)
      }
    }
    
    def set(sig:Boolean){sigVal = sig}
    override def reset() {sigVal = false}
    override def simStarting() { signalObservers() }
    def addObserver(obs: Actor) {
      observers = obs :: observers
    }

    override def toString = "Wire(" + name + ")"
  }

  private object DummyWire extends Wire("dummy")

  abstract class Gate(in1: Wire, in2: Wire, out: Wire)
    extends Simulant {
    def computeOutput(s1: Boolean, s2: Boolean): Boolean
    val delay: Int
    val clock = Circuit.this.clock
    clock.add(this)
    in1.addObserver(this)
    in2.addObserver(this)
    var s1, s2 = false
    def handleSimMessage(msg: Any,time:Int) {
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
    
    override def reset(){s1 = false;s2 = false}
  }
  
  def norGate(in1: Wire, in2: Wire, output: Wire) =
    new Gate(in1, in2, output) {
      val delay = NorGateDelay
      def computeOutput(s1: Boolean, s2: Boolean) = !(s1 || s2)
    }

  def nandGate(in1: Wire, in2: Wire, output: Wire) =
    new Gate(in1, in2, output) {
      val delay = NandGateDelay
      def computeOutput(s1: Boolean, s2: Boolean) = !(s1 && s2)
    }

  def inverter(input: Wire, output: Wire) =
    new Gate(input, DummyWire, output) {
      val delay = InverterDelay
      def computeOutput(s1: Boolean, ignored: Boolean) = !s1
    }

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
  
    /**
   * Reset the circuit.
   * Reset all components in the circuit to false
   * and remove all tracking histories.
   * Need to call set() on each input wire to change
   * the inputs before calling start() to run the simulation
   * again.
   * Only call this method when the simulation is finished.
   */
  def reset()
  {
    clock.reset();
  }
  
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
   * Get the state of the clock.
   * Used to hide the implementation of clock
   * from users of this class.
   */
  def getClockState:Actor.State.Value = clock.getState
  
  def start() { clock ! Start }
}
