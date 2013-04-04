package common.guardedcommands.dataconnectors

import common.Utils
import Utils._
import common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:59
 * To change this template use File | Settings | File Templates.
 */

class GCSync(a: String, b: String, uid: Int) extends GCConnector(List(a,b), uid) {
  protected val av = Var(flowVar(a,uid))
  protected val bv = Var(flowVar(b,uid))

  var constraints = Formula(
    av <-> bv
  )

  if (useData) constraints ++=
    //    av --> (bv := av) //VarAssgn(dataVar(b,uid), dataVar(a,uid))
    (bv := av)

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = constraints
}
