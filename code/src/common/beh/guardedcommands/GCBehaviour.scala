package common.beh.guardedcommands

import common.beh.{Utils, Behaviour}
import common.beh.Utils._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:57
 * To change this template use File | Settings | File Templates.
 */

abstract class GCBehaviour(ends: List[String], uid: Int) extends Behaviour[GCSolution,GuardedCommands](ends,uid) {
  useData = true // data by default

  // adds to "c" the sync constraints wrt the ends shared with "from"
  // TODO: fix based on useData or not.
  def sync(from: AnyRef, c: GuardedCommands) = {
    if (connections contains from) {
      val glue = for ((end,oend,ouid) <- connections(from))
        yield True --> Seq(List(
          Var(flowVar(oend,ouid)) <-> Var(flowVar(end,uid)),
          VarAssgn(dataVar(oend,ouid),dataVar(end,uid))
        ))
      c ++ glue
    }
    else c
  }

  def dataOn(end: String, s: GCSolution): Any = {
    val data = s.varMap.get(Utils.dataVar(end,uid))
    if (data.isDefined) data.get
    else 0
  }

  def noSol = null

  def update(s: GCSolution) {}

  // adds to "c" the border constraints wrt the ends shared with "from"
  def border(from: AnyRef, c: GuardedCommands) = null
}
