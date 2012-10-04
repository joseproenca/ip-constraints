package dreams.connectors

import dreams.Actor
import common.beh.guardedcommands.{GuardedCommands, GCSolution}
import common.beh.guardedcommands.dataconnectors.GCWriter

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
  val uid = hashCode

  val behaviour = new GCWriter("a",uid,(1 to n).toList)

}
