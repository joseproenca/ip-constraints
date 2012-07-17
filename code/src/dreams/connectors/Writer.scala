package dreams.connectors

import dreams.Actor
import common.beh.choco._
import connectors.ChoWriter

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 18:16
 * To change this template use File | Settings | File Templates.
 */

class Writer(var n:Int) extends Actor[ChoSolution, ChoConstraints] {

//  override def init = if (n>0) {  println("starting protocol! ["+n+","+uid+"]"); startProtocol}
//                      else stateIdle
  val uid = hashCode

  val behaviour = new ChoWriter("a",uid,n)

}
