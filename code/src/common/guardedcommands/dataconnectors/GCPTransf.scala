package common.guardedcommands.dataconnectors

import common.{Predicate, Function}
import common.guardedcommands.{Pred, Formula, Var, GCConnector}
import common.Utils._

/**
 * Creates a partial transfer channel: if f is undefined, data cannot pass (filter).
 *
 * Created by jose on 12/04/13.
 */
class GCPTransf[A] (a: String, b: String, uid: Int, f: PartialFunction[A,_]) extends GCConnector(List(a,b), uid) {
  private val av = Var(flowVar(a,uid))
  private val bv = Var(flowVar(b,uid))

  val pred = Predicate(){
    case x: A => f.isDefinedAt(x)
    case _ => false
  }
  val guard = Pred(dataVar(a,uid),pred)

  val func = Function()( f )


  val constraints = Formula(
    bv --> av,
    bv -->  (bv := (func,av)),
    //    bv := av ,
    bv --> guard,
    (av /\ guard) --> bv
  )

  if (!useData) throw new Exception("Partial transfer requires 'useData' option")

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = constraints
}