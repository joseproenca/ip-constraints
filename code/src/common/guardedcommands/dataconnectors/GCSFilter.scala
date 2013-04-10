package common.guardedcommands.dataconnectors

import common.guardedcommands._
import common.Utils._
import common.Predicate
import common.guardedcommands.Neg
import common.guardedcommands.Var
import common.guardedcommands.Pred

/**
 * Creates a SyncFilter: a filter that never loses data (sync channel with a predicate).
 *
 * Created by jose on 10/04/13.
 */
class GCSFilter (a: String, b: String, uid: Int,g: Guard) extends GCFilter(a,b,uid,g) {

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


  override val constraints = Formula(
      bv <-> av,
      bv -->  (bv := av),
      bv --> g
    )
}