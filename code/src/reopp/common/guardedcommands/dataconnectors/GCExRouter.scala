package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:13
 * To change this template use File | Settings | File Templates.
 */

class GCExRouter(a: String, b: String, c: String, uid: Int) extends GCConnector(List(a,b,c), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))
  val cv = Var(flowVar(c,uid))

  private var constraints = Formula(
    av --> (bv or cv),
    (bv or cv) --> av,
    Neg(bv and cv)
  )

  if (useData) constraints ++= Set(
    bv --> VarAssgn(dataVar(b,uid),dataVar(a,uid)),
    cv --> VarAssgn(dataVar(c,uid),dataVar(a,uid))
  )

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = constraints
}