package dreams.connectors

import dreams.Actor
import common.guardedcommands.{GuardedCommands, GCSolution}
import common.guardedcommands.dataconnectors.GCWriter
import common.Utils
import Utils._
import common.guardedcommands.GCConnector.GCBuilder

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 18:16
 * To change this template use File | Settings | File Templates.
 */

class Writer(var n:Int) extends Actor[GCSolution, GuardedCommands] {

//  override def init = if (n>0) {  println("starting protocol! ["+n+","+uid+"]"); startProtocol}
//                      else stateIdle
//  private val uid = hashCode

  val behaviour = new GCWriter("a",uid,(1 to n).toList) {
    private var triedAndFailed = false
    override def update(s: GCSolution) {
      triedAndFailed = !s.hasFlowOn(flowVar(x, uid))
      super.update(s)
    }
    override def isProactive: Boolean = super.isProactive && !triedAndFailed
  }

}
