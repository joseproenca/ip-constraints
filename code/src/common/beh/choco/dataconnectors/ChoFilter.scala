package common.beh.choco.dataconnectors

import common.beh.choco._
import common.beh.choco.ChoConstraints
import common.beh.choco.Var
import choco.kernel.model.variables.integer.IntegerVariable
import choco.kernel.model.constraints.Constraint
import common.beh.Utils._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 12:44
 * To change this template use File | Settings | File Templates.
 */


class ChoFilter(x: String, y: String, uid: Int, p: IntegerVariable => Constraint) extends ChoDataBehaviour(List(x, y), uid) {

  useData = true
  useCC3 = false

  var constraints = ChoConstraints(List(
    // y -> x
    Var(flowVar(y,uid)) --> Var(flowVar(x,uid)),
    // y -> ^x = ^y /\ P(^y)
    Var(flowVar(y,uid)) --> VarEq(dataVar(x,uid),dataVar(y,uid)),
    Var(flowVar(y,uid)) --> FlowPred(p,dataVar(y,uid)),
    // (x /\ P (^x)) -> y
    (Var(flowVar(x,uid)) and FlowPred(p,dataVar(x,uid))) --> Var(flowVar(y,uid))
  ))

  // suggests which ends must have dataflow if "end" has also dataflow
  //  def guessRequirements(end: String) = if (end == x) Set(y) else Set(x)
}
