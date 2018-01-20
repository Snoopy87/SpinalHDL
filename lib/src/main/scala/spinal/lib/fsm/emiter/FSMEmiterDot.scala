package spinal.lib.fsm.emiter

import java.io.{BufferedWriter, File, FileWriter}

import spinal.core.SpinalInfo
import spinal.lib.fsm._


object DotFsm extends FSMEmiterTag

class FSMEmiterDot(config: FsmEmiterConfig)(fsm: StateMachine) extends FSMEmiter(config) {

  def emitFSM(): Unit = {

    val fileName = if(config.nameFile == null) s"${fsm.getName()}" else s"${config.nameFile}"

    def tab = " " * 4

    val transitionStr = fsm.fsmEmiterInfo.transition.map{ case transition =>
      s"${getStateName(transition.start)} -> ${getStateName(transition.end)} [label = ${"\"" + getConditionStrFromScope(transition.scope)+ "\""}];"
    }

    val statesFSM      = fsm.states.filter(s => s.isInstanceOf[StateFsm[_]]).map(s => s"${getStateName(s)} [shape=rectangle, style=${"\"" + "rounded, filled" + "\"" }, fillcolor=yellow];")
    val statesParallel = fsm.states.filter(s => s.isInstanceOf[StateParallelFsm]).map(s => s"${getStateName(s)} [shape=rectangle, style=${"\"" + "rounded, filled" + "\"" }, fillcolor=chartreuse];")
    val statesDelay    = fsm.states.filter(s => s.isInstanceOf[StateDelay]).map(s => s"${getStateName(s)} [style=${"\"" + "filled" + "\""}, fillcolor=chocolate1];")

    val bufferStr    = new StringBuilder()
    bufferStr ++= s"digraph ${fsm.getName()}{\n"
    bufferStr ++= s"${tab}node [style=filled];"
    bufferStr ++= s"${transitionStr.map(trans => s"${tab}${trans}").mkString("\n")} \n"
    bufferStr ++= s"${tab}${"\"" + "State" + "\""} \n"

    if(fsm.stateBoot.isInstanceOf[StateBoot]){
      val bootState = fsm.stateBoot.asInstanceOf[StateBoot]
      if(bootState.autoStart)
        bufferStr ++= s"${tab}${getStateName(bootState)} [shape=invtriangle, style=filled, fillcolor=red]; \n"
    }

    if(statesFSM.nonEmpty){
      bufferStr ++=  s"${statesFSM.map(state => s"${tab}${state}").mkString("\n")} \n"
      bufferStr ++= s"${tab}${"\"" + "Inner FSM" + "\""} [shape=rectangle, style=${"\"" + "rounded, filled" + "\"" }, fillcolor=yellow];\n"
    }

    if(statesParallel.nonEmpty){
      bufferStr ++= s"${statesParallel.map(state => s"${tab}${state}").mkString("\n")} \n"
      bufferStr ++= s"${tab}${"\"" + "Parallel FSM" + "\""} [shape=rectangle, style=${"\"" + "rounded, filled" + "\"" }, fillcolor=chartreuse];\n"
    }

    if(statesDelay.nonEmpty){
      bufferStr ++= s"${statesDelay.map(state => s"${tab}${state}").mkString("\n")} \n"
      bufferStr ++= s"${tab}${"\"" + "Delay State" + "\""} [style=${"\"" + "filled" + "\""}, fillcolor=chocolate1];\n"
    }

    bufferStr ++= "}"

    //println(bufferStr)

    fsm.childStateMachines.foreach{child =>
      config.copy(nameFile = s"${fileName}_${child.getName()}").generate(child.getFSM())
    }

    // Write result into file
    val dotFile = new File(s"${fileName}.dot")
    val bw = new BufferedWriter(new FileWriter(dotFile))
    bw.write(bufferStr.toString())
    bw.close()

    SpinalInfo(s"Generate dot graph into file : ${dotFile.getName}")
  }
}
