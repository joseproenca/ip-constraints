package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 16:44
 * To change this template use File | Settings | File Templates.
 */

class GCLossy(a: String, b: String, uid: Int) extends GCConnector(List(a,b), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))

  var constraints = GuardedCommands(
    bv --> av
  )

  if (useData) constraints +=
      bv --> (bv := av)  //VarAssgn(dataVar(y,uid),dataVar(x,uid))

  if (useCC3) throw new Exception("CC3 not implemented")
}
