package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:59
 * To change this template use File | Settings | File Templates.
 */

class GCSync(a: String, b: String, uid: Int) extends GCBehaviour(List(a,b), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))

  var constraints = GuardedCommands(
    True --> (av <-> bv)
  )

  if (useData) constraints +=
    av --> VarAssgn(dataVar(b,uid), dataVar(a,uid))

  if (useCC3) throw new Exception("CC3 not implemented")
}