package reopp.common

/**
 * Builds new constraints, to be used as implicit arguments (similar to builders in Scala collections).
 *
 * Created by jose on 08/11/12.
 */

abstract class CBuilder[S<: Solution, C <: Constraints[S,C]] {
  /** Creates a new constraint by ''connecting'' two ends, making the act as the same. */
  def sync(end1: String, uid1: Int, end2: String, uid2: Int): C
  /** Empty constraint - identity for constraint composition. */
  def noflow(end: String, uid: Int): C
}
