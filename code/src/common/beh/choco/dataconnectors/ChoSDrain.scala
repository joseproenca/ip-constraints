package common.beh.choco.dataconnectors

import common.beh.Utils._
import common.beh.choco.{VarEq, ChoConstraints, ChoDataBehaviour}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:19
 * To change this template use File | Settings | File Templates.
 */

class ChoSDrain(a:String,b:String,uid:Int) extends ChoDataBehaviour(List(a,b),uid) {

  useData = true
  useCC3 = false

  var constraints = ChoConstraints(
    VarEq(flowVar(a,uid),flowVar(b,uid))
  )

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = if (end == a) Set(b) else Set(a)
}

