package reopp.common.guardedcommands.dataconnectors

import reopp.common.{Predicate, Utils}
import Utils._
import reopp.common.guardedcommands._

/**
 * Creates a Filter: a lossy sync that loses data exactly when a predicate does not hold.
 *
 * Created by jose on 07/06/12.
 */

class GCFilter(a: String, b: String, uid: Int,g: Guard) extends GCConnector(List(a,b), uid) {
  protected val av = Var(flowVar(a,uid))
  protected val bv = Var(flowVar(b,uid))

  /**
   * Build guard (formula) from a Predicate
   * @param a source end
   * @param b sink end
   * @param uid unique channel id
   * @param p predicate
   */
  def this(a: String, b:String, uid: Int, p: Predicate) {
    this(a,b,uid,Pred(dataVar(a,uid),p))
  }

  /**
   * Build guard (formula) from a Predicate
   * @param a source end
   * @param b sink end
   * @param uid unique channel id
   * @param p predicate
   * @param positive if false consider the negation of the predicate
   */
  def this(a: String, b:String, uid: Int, p: Predicate, positive: Boolean) {
    this(a, b, uid, if (positive) Pred(dataVar(a,uid),p)
                    else      Neg(Pred(dataVar(a,uid),p)))
}

  val constraints = Formula(
    bv --> av,
    bv -->  (bv := av), //VarAssgn(dataVar(b,uid),dataVar(a,uid)),
//    bv := av ,
    bv --> g,
    (av /\ g) --> bv
  )

  def getConstraints = constraints

  if (!useData) throw new Exception("Filter requires 'useData' option")

  if (useCC3) throw new Exception("CC3 not implemented")
}

//object GCFilter {
//
//}

class GCTFilter[A](a: String, b: String, uid: Int,filter: (A) => Boolean)
  extends GCFilter(a, b, uid,
    Pred(dataVar(a,uid),Predicate()(filter)))