package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.Utils._
import reopp.common.Predicate
import reopp.common.guardedcommands.Neg
import reopp.common.guardedcommands.Var
import reopp.common.guardedcommands.Pred

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


  override def getConstraints = Formula(
      b <-> a,
      b -->  (b := a),
      b --> g
    )
}