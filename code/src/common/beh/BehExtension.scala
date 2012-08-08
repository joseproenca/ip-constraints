package common.beh

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 08/08/12
 * Time: 16:29
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class BehExtension {
  def useCC3: Boolean
  def useData: Boolean
}

object DataX extends BehExtension{ val useCC3 = false; val useData = true}
object CC3X extends BehExtension { val useCC3 = true; val useData = false}
object DataCC3X extends BehExtension { val useCC3 = true; val useData = true}

