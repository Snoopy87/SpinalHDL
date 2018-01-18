/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.fsm


import spinal.core._
import spinal.core.internals._
import spinal.lib._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class StateMachineEnum extends SpinalEnum


/**
  * State machine
  *
  * @example {{{
  *   val fsm = new StateMachine {
  *     val sIdle: State = StateEntryPoint{
  *       ...
  *     }
  *     val sState1: State = new State {
  *       whenIsActive{
  *         goto(sIdle)
  *       }
  *     }
  *     ...
  *   }
  * }}}
  */
class StateMachine extends Area with StateMachineAccessor with ScalaLocated {

  // GUI
  case class GUI_Transition(start: State, end: State, scope: ScopeStatement){

    def condition : String = {
        scope.parentStatement match{
          case x : WhenStatement if scope == x.whenTrue  => dispatchExpression(x.cond)
          case x : WhenStatement if scope == x.whenFalse => s"!(${dispatchExpression(x.cond)})"
          case _ => " ?? "
        }
    }


    override def toString: String = s"${start} - ${condition}-> ${end}"
  }
  case class GUI_Info(
                     transition: ArrayBuffer[GUI_Transition] = new ArrayBuffer[GUI_Transition]()
                     )
  //val gotoBuffer = new ArrayBuffer[String]()
  val guiInfo = GUI_Info()


  var inGeneration = false
  val alwaysTasks  = ArrayBuffer[() => Unit]()

  def always(doThat: => Unit): this.type = {
    alwaysTasks += (() => doThat)
    this
  }

  def setEncoding(encoding: SpinalEnumEncoding): Unit = enumDefinition.defaultEncoding = encoding

  @dontName val postBuildTasks = ArrayBuffer[() => Unit]()

  val cache = mutable.HashMap[Any,Any]()
  val enumDefinition = new StateMachineEnum
  var stateReg  : enumDefinition.C = null
  var stateNext : enumDefinition.C = null
  var stateBoot : State = null
  override val wantExit  = False.allowPruning()
  var autoStart = true

  @dontName var parentStateMachine: StateMachineAccessor = null
  @dontName val childStateMachines = mutable.Set[StateMachineAccessor]()
  @dontName val states   = ArrayBuffer[State]()
  val stateToEnumElement = mutable.HashMap[State, enumDefinition.E]()
  @dontName var entryState: State = null

  def enumOf(state: State) = {
    checkState(state)
    stateToEnumElement(state)
  }

  def checkState(state: State) = assert(state.getStateMachineAccessor == this, s"A state machine ($this)is using a state ($state) that come from another state machine.\n\nState machine defined at ${this.getScalaLocationLong}\n State defined at ${state.getScalaLocationLong}")

  override def build(): Unit = {

    inGeneration = true
    childStateMachines.foreach(_.build())
    stateBoot = new StateBoot(autoStart).setName("boot")
    stateReg  = Reg(enumDefinition())
    stateNext = enumDefinition()

    OwnableRef.proposal(stateBoot, this)
    OwnableRef.proposal(stateReg, this)
    OwnableRef.proposal(stateNext, this)

    stateReg.setPartialName("stateReg")
    stateNext.setPartialName("stateNext")

    for(state <- states){
      checkState(state)
      val enumElement = enumDefinition.newElement(if(state.isNamed) state.getName() else null)
      stateToEnumElement += (state -> enumElement)
    }

    stateReg init(enumOf(this.stateBoot))
    stateReg := stateNext

    val stateRegOneHotMap  = states.map(state => (state -> (stateReg === enumOf(state)))).toMap
    val stateNextOneHotMap = states.map(state => (state -> (stateNext === enumOf(state)))).toMap

    stateNext := stateReg

    switch(stateReg){
      for(state <- states){
        if(state == stateBoot) default {
          state.whenActiveTasks.foreach(_())
        } else is(enumOf(state)) {
          state.whenActiveTasks.foreach(_())
        }
      }
    }

    switch(stateNext){
      for(state <- states){
        if(state == stateBoot) default {
          state.whenIsNextTasks.foreach(_())
        } else is(enumOf(state)) {
          state.whenIsNextTasks.foreach(_())
        }
      }
    }


    for(state <- states){
      when(!stateRegOneHotMap(state) && stateNextOneHotMap(state)){
        state.onEntryTasks.foreach(_())
      }
      when(stateRegOneHotMap(state) && !stateNextOneHotMap(state)){
        state.onExitTasks.foreach(_())
      }
      when(!stateRegOneHotMap(state)){
        state.whenInactiveTasks.foreach(_())
      }
    }

    alwaysTasks.foreach(_())
    postBuildTasks.foreach(_())

    // GUI
    guiInfo.transition.foreach(println)
  }

  Component.current.addPrePopTask(() => {
    if(parentStateMachine == null)
      build()
  })


  override def setEntry(state: State): Unit = {
    assert(entryState == null, "Entry point already set !")
    entryState = state
  }

  override def getEntry(): State = entryState

  override def goto(nextState: State, currentState: State = null): Unit = {
    assert(inGeneration, "You can't use the 'goto' function there. Maybe you should use an always{.. goto(x) ..} block ?")
    stateNext := enumOf(nextState)

    // GUI

    // Get the current scope
    val currentScope = GlobalData.get.currentScope
//    var expression : Expression = null
//
//    currentScope.parentStatement match {
//      case x : WhenStatement if currentScope == x.whenTrue  =>
////        println("-----> " + dispatchExpression(x.cond))
//        expression = x.cond
//      case x : WhenStatement if currentScope == x.whenFalse  =>
////        println(dispatchExpression(x.cond))
//        expression = x.cond
//      case _ =>
//    }


    guiInfo.transition += GUI_Transition(currentState, nextState, currentScope)
  }

  override def isActive(state: State): Bool = {
    val ret = Bool
    postBuildTasks += {() => {
      ret := stateReg === enumOf(state)
    }}
    ret
  }

  override def isEntering(state: State): Bool = {
    val ret = Bool
    postBuildTasks += {() => {
      ret := stateNext === enumOf(state) && stateReg =/= enumOf(state)
    }}
    ret
  }

  override def add(state: State): Int = {
    if (state.isInstanceOf[StateBoot]) {
      states.+=:(state)
    } else {
      states += state
    }
    states.length-1
  }

  override def add(stateMachine: StateMachineAccessor): Unit = {
    childStateMachines += stateMachine
    stateMachine.setParentStateMachine(this)
  }

  override def startFsm(currentState: State): Unit = {
    if(entryState == null)
      globalData.pendingErrors += (() => (s"$this as no entry point set. val yourState : State = new State with EntryPoint{...}   should solve the situation at \n${getScalaLocationLong}"))
    else
      goto(entryState, currentState)
  }

  override def exitFsm(currentState: State): Unit = {
    wantExit := True
    goto(stateBoot, currentState)
  }

  @dontName implicit val implicitFsm = this

  override def disableAutoStart(): Unit = autoStart = false

  override def setParentStateMachine(parent: StateMachineAccessor): Unit = parentStateMachine = parent

  override def cacheGet(key: Any): Option[Any] = cache.get(key)
  override def cachePut(key: Any, value: Any): Unit = cache.put(key, value)

  override def isStateNextBoot(): Bool = stateNext === enumOf(stateBoot)
  override def isStateRegBoot():  Bool = stateReg === enumOf(stateBoot)

  // GUI WIP
  def dispatchExpression(e: Expression):  String = e match {

    case  e: BaseType                                => s"${e.getName()}"

    //bool
    case  e: Operator.Bool.Equal                     => s"(${dispatchExpression(e.left)} == ${dispatchExpression(e.right)})"
    case  e: Operator.Bool.NotEqual                  => s"(${dispatchExpression(e.left)} != ${dispatchExpression(e.right)})"

    case  e: Operator.Bool.And                       => s"(${dispatchExpression(e.left)} & ${dispatchExpression(e.right)})"
    case  e: Operator.Bool.Or                        => s"(${dispatchExpression(e.left)} | ${dispatchExpression(e.right)})"
    case  e: Operator.Bool.Xor                       => s"(${dispatchExpression(e.left)} ^ ${dispatchExpression(e.right)})"

    case _ => "???"
  }

  private def getNameElseThrows(e: BaseType): String = {
    e.getName(null) match {
      case null =>  throw new Exception("Internal error")
      case name =>  name
    }
  }


}
