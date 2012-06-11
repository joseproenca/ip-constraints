package common.beh.choco.dataconnectors

import common.beh.Utils._
import common.beh.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:15
 * To change this template use File | Settings | File Templates.
 */

class ChoSync(a:String,b:String,uid:Int) extends ChoDataBehaviour(List(a,b),uid) {

  var constraints = ChoConstraints(List(
    VarEq(flowVar(a,uid),flowVar(b,uid)),
    Var(flowVar(a,uid)) --> VarEq(dataVar(a,uid),dataVar(b,uid))
  ))


  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = if (end == a) Set(b) else Set(a)
}

