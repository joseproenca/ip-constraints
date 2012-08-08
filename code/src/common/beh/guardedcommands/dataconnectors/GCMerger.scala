package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:21
 * To change this template use File | Settings | File Templates.
 */

class GCMerger(a: String, b: String, c: String, uid: Int) extends GCBehaviour(List(a,b,c), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))
  val cv = Var(flowVar(c,uid))

  var constraints = GuardedCommands(
    cv --> (av \/ bv),
    (av \/ bv) --> cv,
    !(av and bv))

  if (useData) constraints ++= Set(
    av --> (cv := av),
    bv --> (cv := av))
  //    av --> VarAssgn(dataVar(c,uid),dataVar(a,uid)),
  //    bv --> VarAssgn(dataVar(c,uid),dataVar(b,uid))

  if (useCC3) throw new Exception("CC3 not implemented")

}