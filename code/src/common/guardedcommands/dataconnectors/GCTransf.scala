package common.guardedcommands.dataconnectors

import common.guardedcommands._
import common.{Function, Utils}
import Utils._
import common.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/07/12
 * Time: 15:52
 * To change this template use File | Settings | File Templates.
 */

class GCTransf (a: String, b: String, uid: Int, f: Function) extends GCConnector(List(a,b), uid) {
  private val av = Var(flowVar(a,uid))
  private val bv = Var(flowVar(b,uid))

  var constraints = GuardedCommands(
    av <-> bv
  )

  if (useData) constraints ++=
    av --> (bv := (f,av))  // FunAssgn(dataVar(b,uid), dataVar(a,uid), f)

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = constraints
}
