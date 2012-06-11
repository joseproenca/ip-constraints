package common.beh.guardedcommands

import common.beh.Behaviour
import common.beh.Utils._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:57
 * To change this template use File | Settings | File Templates.
 */

abstract class GCBehaviour(ends: List[String], uid: Int) extends Behaviour[GCSolution,GuardedCommands](ends,uid) {
  // adds to "c" the sync constraints wrt the ends shared with "from"
  def sync(from: AnyRef, c: GuardedCommands) = {
    if (connections contains from) {
      val glue = for ((end,oend,ouid) <- connections(from))
        yield True --> Seq(List(
          SGuard(Var(flowVar(oend,ouid)) <-> Var(flowVar(end,uid))),
          VarAssgn(dataVar(oend,ouid),dataVar(end,uid))
        ))
      c ++ glue
    }
    else c
  }

  def dataOn(end: String, s: GCSolution) = null

  def noSol = null

  def update(s: GCSolution) {}

  // adds to "c" the border constraints wrt the ends shared with "from"
  def border(from: AnyRef, c: GuardedCommands) = null
}
