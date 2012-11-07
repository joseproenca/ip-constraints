package common.beh.choco.connectors

import common.beh.Utils._
import common.beh.choco._


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:57
 * To change this template use File | Settings | File Templates.
 */

class ChoSync(x:String,y:String,uid:Int) extends ChoConnector(List(x,y),uid) {

  useData = false
  useCC3 = false

  def getConstraints = ChoConstraints(VarEq(flowVar(x,uid),flowVar(y,uid)))

  // suggests which ends must have dataflow if "end" has also dataflow
//  def guessRequirements(end: String) = if (end == x) Set(y) else Set(x)
}
