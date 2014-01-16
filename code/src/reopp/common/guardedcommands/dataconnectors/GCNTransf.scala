package reopp.common.guardedcommands.dataconnectors

import reopp.common.Function
import reopp.common.guardedcommands.{Formula, Var, GCConnector}
import reopp.common.Utils._

/**
 * N-ary transfer channel: receives data from n ports, calculates f(a1,...,an), and returns the result via b.
 *
 * Created by jose on 10/04/13.
 */
class GCNTransf (ans: List[String], bn: String, uid: Int, f: Function) extends GCConnector(ans ++ List(bn), uid) {
  private val as = ans.map(a => Var(flowVar(a,uid)))
  private val b  = Var(flowVar(bn,uid))

  var constraints = Formula(
    as.map(a =>  st2GC(a <-> b)) // st2GC is an implicit conversion, but needed because typechecking is not smart enough.
  )

  if (useData) constraints ++=
    b --> (b := (f,as))

  if (useCC3) throw new Exception("CC3 not implemented")


  def getConstraints = constraints
}
