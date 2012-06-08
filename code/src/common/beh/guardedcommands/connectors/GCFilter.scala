package common.beh.guardedcommands.connectors

import common.beh.choco.Utils._
import common.beh.guardedcommands._
import common.beh.choco.dataconnectors.Predicate

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 17:21
 * To change this template use File | Settings | File Templates.
 */

class GCFilter(x: String, y: String, uid: Int,p: Predicate) extends GCBehaviour(List(x,y), uid) {

  var constraints = GuardedCommands(Set(
    // y -> x
    Var(flowVar(y,uid)) --> SGuard(Var(flowVar(x,uid))),
    // y -> ^y = ^x /\ P(^x)
    Var(flowVar(y,uid)) --> VarAssgn(dataVar(y,uid),dataVar(x,uid)),
    Var(flowVar(y,uid)) --> SGuard(Pred(p,dataVar(x,uid))),
    // (x /\ P (^x)) -> y
    (Var(flowVar(x,uid)) and Pred(p,dataVar(x,uid))) --> SGuard(Var(flowVar(y,uid)))
  ))

}