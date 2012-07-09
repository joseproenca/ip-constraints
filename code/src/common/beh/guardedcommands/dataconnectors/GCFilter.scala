package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 17:21
 * To change this template use File | Settings | File Templates.
 */

class GCFilter(a: String, b: String, uid: Int,g: Guard) extends GCBehaviour(List(a,b), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))

  var constraints = GuardedCommands(Set(
    // b -> a
    bv --> SGuard(av),
    // b -> ^b = ^a /\ P(^a)
    bv --> VarAssgn(dataVar(b,uid),dataVar(a,uid)),
    bv --> SGuard(g),
    // (a /\ P (^a)) -> b
    (av and g) --> SGuard(bv)
  ))

  if (!useData) throw new Exception("Filter requires 'useData' option")

  if (useCC3) throw new Exception("CC3 not implemented")
}