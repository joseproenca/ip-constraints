package reopp.dreams.connectors

import reopp.dreams.Actor
import reopp.common.guardedcommands.{Formula, GCSolution}
import reopp.common.guardedcommands.dataconnectors.GCWriter
import reopp.common.{OptionSol, Utils}
import Utils._
import reopp.common.guardedcommands.GCConnector.GCBuilder

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 18:16
 * To change this template use File | Settings | File Templates.
 */

class Writer(var n:Int) extends Actor[GCSolution, Formula] {

//  override def init = if (n>0) {  println("starting protocol! ["+n+","+uid+"]"); startProtocol}
//                      else stateIdle
//  private val uid = hashCode

  val behaviour = new GCWriter("a",(1 to n).map(Int.box(_)).toList) {
    private var triedAndFailed = false
    override def update(s: OptionSol[GCSolution]) {
      triedAndFailed = !s.isDefined
      if (!triedAndFailed) triedAndFailed = !s.get.hasFlowOn(mkFlowVar(x))
      super.update(s)
    }
    override def isProactive: Boolean = super.isProactive && !triedAndFailed
  }

}
