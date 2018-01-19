package spinal.lib.fsm.emiter

import java.io.{BufferedWriter, File, FileWriter}

import spinal.lib.fsm._

class FSMEmiterDot(fsm: StateMachine, info: FSMEmiterInfo) extends FSMEmiter {

  def emitFSM(nameFile: String = s"${fsm.getName()}.dot"): Unit = {

    def tab = " " * 4

    val transitionStr = info.transition.map{ case transition =>
      s"${getNameState(transition.start)} -> ${getNameState(transition.end)} [label = ${"\"" + getConditionStrFromScope(transition.scope)+ "\""}];"
    }

    val fillStateFSM   = fsm.states.filter(s => s.isInstanceOf[StateFsm[_]]).map(s => s"${getNameState(s)} [shape=rectangle, style=${"\"" + "rounded, filled" + "\"" }, fillcolor=yellow];")
    val fillStatePara  = fsm.states.filter(s => s.isInstanceOf[StateParallelFsm]).map(s => s"${getNameState(s)} [shape=rectangle, style=${"\"" + "rounded, filled" + "\"" }, fillcolor=chartreuse];")
    val fillStateDelay = fsm.states.filter(s => s.isInstanceOf[StateDelay]).map(s => s"${getNameState(s)} [style=${"\"" + "filled" + "\""}, fillcolor=chocolate1];")

    val bufferStr    = new StringBuilder()
    bufferStr ++= s"digraph ${fsm.getName()}{\n"
    bufferStr ++= s"${transitionStr.map(trans => s"${tab}${trans}").mkString("\n")} \n"

    if(fsm.stateBoot.isInstanceOf[StateBoot]){
      val bootState = fsm.stateBoot.asInstanceOf[StateBoot]
      if(bootState.autoStart)
        bufferStr ++= s"${tab}${getNameState(bootState)} [shape=triangle]; \n"
    }

    if(fillStateFSM.nonEmpty){
      bufferStr ++=  s"${fillStateFSM.map(state => s"${tab}${state}").mkString("\n")} \n"
    }

    if(fillStatePara.nonEmpty){
      bufferStr ++= s"${fillStatePara.map(state => s"${tab}${state}").mkString("\n")} \n"
    }

    if(fillStateDelay.nonEmpty){
      bufferStr ++= s"${fillStateDelay.map(state => s"${tab}${state}").mkString("\n")} \n"
    }

    bufferStr ++= "}"


    println(bufferStr)

    fsm.childStateMachines.foreach(_.getStateMachine().emitMetaData())

    // Write result into file
    val bw = new BufferedWriter(new FileWriter(new File(nameFile)))
    bw.write(bufferStr.toString())
    bw.close()
  }




}
