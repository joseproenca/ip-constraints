package common.beh

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 08/11/12
 * Time: 12:22
 * To change this template use File | Settings | File Templates.
 */
abstract class CBuilder[S<: Solution, C <: Constraints[S,C]] {
  def sync(end1: String, uid1: Int, end2: String, uid2: Int): C
  def noflow(end: String, uid: Int): C
}
