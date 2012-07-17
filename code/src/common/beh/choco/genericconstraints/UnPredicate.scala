package common.beh.choco.genericconstraints

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 10:47
 * To change this template use File | Settings | File Templates.
 */
abstract class UnPredicate {
  def check(x: Any): Boolean

//  def opposite = new UnPred {
//    def check(x: Any) = this.check(x)
//  }
}

