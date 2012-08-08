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

class GCLossy(x: String, y: String, uid: Int) extends GCBehaviour(List(x,y), uid) {
  var constraints = GuardedCommands(
    Var(flowVar(y,uid)) --> Var(flowVar(x,uid))
  )

  if (useData) constraints ++= Set(
    Var(flowVar(y,uid)) -->
      VarAssgn(dataVar(y,uid),dataVar(x,uid))
  )

  if (useCC3) throw new Exception("CC3 not implemented")
}
