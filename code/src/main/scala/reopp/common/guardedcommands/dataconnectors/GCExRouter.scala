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

class GCExRouter(a: String, b: String, c: String) extends GCConnector(List(a,b,c)) {
//  val a = Var(flowVar(a,uid))
//  val b = Var(flowVar(b,uid))
//  val c = Var(flowVar(c,uid))

  private def constraints = Formula(
    a --> (b or c),
    (b or c) --> a,
    Neg(b and c)
  )

  private def dataConstraints =
    constraints ++ Set(
      b --> (b := a), //VarAssgn(dataVar(b,uid),dataVar(a,uid)),
      c --> (c := a) // VarAssgn(dataVar(c,uid),dataVar(a,uid))
    )

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = if (useData) dataConstraints else constraints
}