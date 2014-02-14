package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:21
 * To change this template use File | Settings | File Templates.
 */

class GCMerger(a: String, b: String, c: String, uid: Int) extends GCConnector(List(a,b,c), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))
  val cv = Var(flowVar(c,uid))

  private var constraints = Formula(
    cv --> (av \/ bv),
    (av \/ bv) --> cv,
    !(av and bv))

  if (useData) constraints ++= Set(
    av --> (cv := av),
    bv --> (cv := bv))
  //    av --> VarAssgn(dataVar(c,uid),dataVar(a,uid)),
  //    bv --> VarAssgn(dataVar(c,uid),dataVar(b,uid))

  def getConstraints = constraints

  if (useCC3) throw new Exception("CC3 not implemented")

}