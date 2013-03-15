package common.guardedcommands.dataconnectors

import common.Utils
import Utils._
import common.guardedcommands._
import scala.None

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:43
 * To change this template use File | Settings | File Templates.
 */

class GCFifo(a: String, b: String, var data: Option[Any], uid: Int = 0) extends GCConnector(List(a,b), uid) {

//  def this(a: String, b: String, dt: Option[Int], uid: Int) = this(a,b, dt.map(Int.box(_)),uid)
  def this(a: String, b: String, uid: Int) = this(a,b, scala.None:Option[Any],uid)


  val av = mkVar(a,uid)
  val bv = mkVar(b,uid)

  val emptyFifo = GuardedCommands(!bv)

  def fullFifo =
    if (useData) GuardedCommands(
        !av,
        bv --> (bv := data.get)
      )
    else if (useCC3) throw new Exception("CC3 not implemented")
    else GuardedCommands(Neg(av))


  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: GCSolution) {
    if (s hasFlowOn flowVar(a,uid)) {
      // update state
      data = Some(s(dataVar(a,uid)))
      // update constraints
      //constraints = fullFifo  //---- constraints automatically updated with state.
      // println("FIFO: FLOW IN!")
      // notifyflow()
    }
    else if (s hasFlowOn flowVar(b,uid)) {
      // update state
      data = None
      // update constraints
      //constraints = emptyFifo //---- constraints automatically updated with state.
      // println("FIFO: FLOW OUT!")
      // notifyflow()
    }
  }

  // update state not implemented
}
