package common.beh.choco.dataconnectors

import common.beh.choco._
import common.beh.choco.Utils._


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 10:50
 * To change this template use File | Settings | File Templates.
 */

class ChoLossy(x: String, y: String, uid: Int) extends ChoDataBehaviour(List(x, y), uid) {
  //  var constraints = ChoConstraints(And(
  //    Impl(Var(ConstrBuilder.flowVar(y,uid)), Var(ConstrBuilder.flowVar(x,uid))),
  //    (Var(ConstrBuilder.flowVar(x,uid)))
    //  ))
  var constraints = ChoConstraints(List(
    Impl(Var(flowVar(y, uid)), Var(flowVar(x, uid))),
    VarEq(dataVar(x,uid),dataVar(y,uid))
  ))

  // suggests which ends must have dataflow if "end" has also dataflow
  //  def guessRequirements(end: String) = if (end == x) Set(y) else Set(x)
}
