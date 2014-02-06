package reopp.common.guardedcommands.dataconnectors

import reopp.common.Function
import reopp.common.guardedcommands.{Formula, Var, GCConnector}
import reopp.common.Utils._
import reopp.common.guardedcommands.GuardedCom

/**
 * N-ary transfer channel: receives data from n ports, calculates f(a1,...,an), and returns the result via b.
 *
 * Created by jose on 10/04/13.
 */
class GCNTransf (ans: List[String], bn: String, uid: Int, f: Function) extends GCConnector(ans ++ List(bn), uid) {
  private val as = ans.map(mkVar(_))
  private val b:Var = "b" // mkVar(bn, uid) // Var(flowVar(bn,uid)) // implicit conversion

  var constraints = Formula(
    for (a<-as) yield (a <-> b):GuardedCom
  )

  if (useData) constraints ++=
    b --> (b := (f,as))

  if (useCC3) throw new Exception("CC3 not implemented")


  def getConstraints = constraints
}
