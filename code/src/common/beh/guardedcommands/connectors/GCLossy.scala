package common.beh.guardedcommands.connectors

import common.beh.choco.Utils._
import common.beh.guardedcommands._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 16:44
 * To change this template use File | Settings | File Templates.
 */

class GCLossy(x: String, y: String, uid: Int) extends GCBehaviour(List(x,y), uid) {
  var constraints = GuardedCommands(Set(
    Var(flowVar(y,uid)) --> Seq(List(
      SGuard(Var(flowVar(x,uid))),
      VarAssgn(dataVar(y,uid),dataVar(x,uid)))
  )))
}
