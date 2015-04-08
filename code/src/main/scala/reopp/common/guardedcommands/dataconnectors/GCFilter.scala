package reopp.common.guardedcommands.dataconnectors

import reopp.common.{Predicate, Utils}
import Utils._
import reopp.common.guardedcommands._
import reopp.common.IntPredicate

/**
 * Creates a Filter: a lossy sync that loses data exactly when a predicate does not hold.
 *
 * Created by jose on 07/06/12.
 */

class GCFilter(a: String, b: String,p: Predicate, positive:Boolean = true) extends GCConnector(List(a,b)) {

//  /**
//   * Build guard (formula) from a Predicate
//   * @param a source end
//   * @param b sink end
//   * @param uid unique channel id
//   * @param p predicate
//   */
//  def this(a: String, b:String, uid: Int, p: Predicate) {
//    this(a,b,uid,Pred(dataVar(a,uid),p))
//  }

//  /**
//   * Build guard (formula) from a Predicate
//   * @param a source end
//   * @param b sink end
//   * @param uid unique channel id
//   * @param p predicate
//   * @param positive if false consider the negation of the predicate
//   */
//  def this(a: String, b:String, uid: Int, p: Predicate, positive: Boolean) {
//    this(a, b, uid, if (positive) Pred(dataVar(a,uid),p)
//                    else      Neg(Pred(dataVar(a,uid),p)))
//}
  
  protected def guard: Guard = if (positive) Pred(a.data,p)
                               else      Neg(Pred(a.data,p))

  def getConstraints = Formula(
    b --> a,
    b -->  (b := a), //VarAssgn(dataVar(b,uid),dataVar(a,uid)),
//    bv := av ,
    b --> guard,
    (a /\ guard) --> b
  )
  
  if (!useData) throw new Exception("Filter requires 'useData' option")

  if (useCC3) throw new Exception("CC3 not implemented")
}

//object GCFilter {
//
//}

class GCTFilter[A](a: String, b: String, filter: (A) => Boolean)
  extends GCFilter(a, b, Predicate()(filter))

class GCIFilter[A](a: String, b: String, p: IntPredicate, positive:Boolean = true)
  extends GCFilter(a, b,  p, positive) {
  override def guard =  if (positive) IntPred(a.data,p)
                        else      Neg(IntPred(a.data,p))
}

class GCGenFilter(a: String, b:String, gfunc: Var => Guard)
	extends GCFilter(a,b,null) {
	override def guard: Guard = gfunc.apply( Var(Utils.mkDataVar(a)) )
  }
    

