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

trait StateMachineAccessor {

  def setEntry(state: State): Unit
  def getEntry(): State

  def isActive(state: State): Bool
  def isEntering(state: State): Bool

  def goto(nextState: State, currentState: State): Unit

  def add(state: State): Int
  def add(stateMachine: StateMachineAccessor): Unit

  def startFsm(currentState: State): Unit
  def exitFsm(currentState: State): Unit
  def wantExit(): Bool

  def disableAutoStart(): Unit

  def getName(): String

  def build(): Unit

  def setParentStateMachine(parent: StateMachineAccessor): Unit

  def cacheGet(key: Any): Option[Any]
  def cachePut(key: Any, value: Any): Unit

  def isStateNextBoot(): Bool
  def isStateRegBoot(): Bool

  def cacheGetOrElseUpdate(key: Any, op: => Any): Any = {
    cacheGet(key) match{
      case Some(value) => value
      case None  =>
        val value = op
        cachePut(key,value)
        value
    }
  }
}

