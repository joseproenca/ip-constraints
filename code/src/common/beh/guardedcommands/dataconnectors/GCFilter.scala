package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._
import common.beh.UnPredicate

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 17:21
 * To change this template use File | Settings | File Templates.
 */

class GCFilter(a: String, b: String, uid: Int,g: Guard) extends GCConnector(List(a,b), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))

  /**
   * Build guard (formula) from a UnPredicate
   * @param a source end
   * @param b sink end
   * @param uid unique channel id
   * @param p predicate
   */
  def this(a: String, b:String, uid: Int, p: UnPredicate) {
    this(a,b,uid,Pred(dataVar(a,uid),p))
  }

  /**
   * Build guard (formula) from a UnPredicate
   * @param a source end
   * @param b sink end
   * @param uid unique channel id
   * @param p predicate
   * @param positive if false consider the negation of the predicate
   */
  def this(a: String, b:String, uid: Int, p: UnPredicate, positive: Boolean) {
    this(a, b, uid, if (positive) Pred(dataVar(a,uid),p)
                    else      Neg(Pred(dataVar(a,uid),p)))
}

  def getConstraints = GuardedCommands(
    bv --> av,
    bv -->  (bv := av), //VarAssgn(dataVar(b,uid),dataVar(a,uid)),
//    bv := av ,
    bv --> g,
    (av /\ g) --> bv
  )

  if (!useData) throw new Exception("Filter requires 'useData' option")

  if (useCC3) throw new Exception("CC3 not implemented")
}

//object GCFilter {
//
//}