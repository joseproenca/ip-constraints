package reopp.common.guardedcommands.dataconnectors

import reopp.common.{Predicate, Function}
import reopp.common.guardedcommands.{Pred, Formula, Var, GCConnector}
import reopp.common.Utils._

/**
 * Creates a partial transfer channel: if f is undefined, data cannot pass (filter).
 *
 * Created by jose on 12/04/13.
 */
class GCPTransf[A] (a: String, b: String, uid: Int, f: PartialFunction[A,_]) extends GCConnector(List(a,b), uid) {

  private val pred = Predicate(){
    case x: A => f.isDefinedAt(x)
    case _ => false
  }
  private def guard = Pred(dataVar(a,uid),pred)

  private val func = Function()( f )


  def getConstraints = Formula(
    b --> a,
    b -->  (b := (func,a)),
    //    bv := av ,
    b --> guard,
    (a /\ guard) --> b
  )

  if (!useData) throw new Exception("Partial transfer requires 'useData' option")

  if (useCC3) throw new Exception("CC3 not implemented")
}