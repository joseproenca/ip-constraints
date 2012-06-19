package common.beh.choco.dataconnectors

import common.beh.choco.{ChoConstraints, ChoDataBehaviour}
import common.beh.Utils._
import common.beh.choco.VarEq

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 19/06/12
 * Time: 19:48
 * To change this template use File | Settings | File Templates.
 */

class ChoSSpout(a:String,b:String,uid:Int) extends ChoDataBehaviour(List(a,b),uid) {

  var constraints = ChoConstraints(
    VarEq(flowVar(a,uid),flowVar(b,uid))
  )

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = if (end == a) Set(b) else Set(a)
}