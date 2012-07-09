package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:13
 * To change this template use File | Settings | File Templates.
 */

class GCExRouter(a: String, b: String, c: String, uid: Int) extends GCBehaviour(List(a,b,c), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))
  val cv = Var(flowVar(c,uid))

  var constraints = GuardedCommands(Set(
    av --> SGuard(bv or cv),
    (bv or cv) --> SGuard(av),
    True --> SGuard(Neg(bv and cv))
  ))

  if (useData) constraints ++= Set(
    bv --> VarAssgn(dataVar(b,uid),dataVar(a,uid)),
    cv --> VarAssgn(dataVar(c,uid),dataVar(a,uid))
  )

  if (useCC3) throw new Exception("CC3 not implemented")

}