package spinal.debugger.gui

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.{JString, JValue}
import spinal.core.SpinalError

import scala.collection.mutable.ArrayBuffer


object BusManager {
  sealed trait BusManagerStates
  case object Boot extends BusManagerStates
  case object WaitPassport extends BusManagerStates
  case object Running extends BusManagerStates
}
case class UidPeripheral(clazz: String, kind: String, uid: String)

class BusManager(hal: BytePacketHal, guiTreeViewManager: IGuiTreeViewManager, reports: Seq[JValue]) {
  implicit def b(x: Int) = x.toByte

  val thread = new Thread with IBytePacketHalObserver {

    import BusManager._

    var state: BusManagerStates = Boot
    val passports = ArrayBuffer[Seq[Byte]]()
    hal.addObserver(this)
    hal.open

    override def run {
      state = WaitPassport
      passportCall
      Thread.sleep(400)
      for (passport <- passports) {
        implicit val formats = DefaultFormats
        val addressString = BigInt(passport.takeRight(4).reverseIterator.toArray).toString(10)
        val report = reports.filter(report => {
          report.\("uid") match{
            case jString : JString => jString.values == addressString
            case _ => false
          }
        })

        if(report.length > 1) throw SpinalError("Multiple UID with same value ???")

        report.foreach(r => {
          try {
            val periphReport = r.extract[UidPeripheral]
            if(periphReport.clazz == "uidPeripheral"){
              guiTreeViewManager.add(Seq(periphReport.kind,periphReport.uid))
            }
          } catch {
            case e : Exception =>
          }
        })

      }
      state = Running
    }

    override def packetHalEvent(in: Seq[Byte]): Unit = {
      state match {
        case Boot =>
        case WaitPassport => {
          passports += in
        }
        case Running =>
        case _ =>
      }
    }

    def passportCall: Unit = {
      hal.tx(Seq(0xFF, 0xFF, 0xFF, 0xFF))
    }
  }

  thread.start
}