package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._

import scala.util.Random


class AhbLite3ArbiterComponent(config: AhbLite3Config, size: Int, roundRobinArbiter: Boolean) extends Component{

  val io = new Bundle{
    val busIn  = Vec(slave(AhbLite3(config)), size)
    val busOut = master(AhbLite3(config))
  }

  val arbiter = AhbLite3Arbiter(config, size, roundRobinArbiter)
  (arbiter.io.inputs, io.busIn).zipped.foreach(_ <> _)
  io.busOut <> arbiter.io.output
}


class SpinalSimAhbLite3Interconnect extends FunSuite {


  def testArbiter(config : AhbLite3Config, size: Int, roundRobinArbiter: Boolean, description : String = ""): Unit = {

    val compiledRTL = SimConfig.allOptimisation.withWave.compile(rtl = new AhbLite3ArbiterComponent(config, size, roundRobinArbiter))
    compiledRTL.doSim(description){ dut =>

      dut.clockDomain.forkStimulus(period=10)

      dut.io.busIn.foreach{ bus =>
        bus.HADDR     #= 0
        bus.HSEL      #= false
        bus.HREADY    #= true
        bus.HWRITE    #= false
        bus.HSIZE     #= 0
        bus.HBURST    #= 0
        bus.HPROT     #= 0
        bus.HTRANS    #= 0
        bus.HMASTLOCK #= false
      }

      dut.clockDomain.waitSampling(10)

      SimTimeout(1000*10000)
    }
  }



  test("Arbitrer"){
    testArbiter(AhbLite3Config(32, 32), 3, true, "BasisArbiter")
  }
}
