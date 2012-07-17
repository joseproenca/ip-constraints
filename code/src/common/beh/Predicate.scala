package common.beh


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 10/07/12
 * Time: 14:14
 * To change this template use File | Settings | File Templates.
 */

@deprecated
abstract class Predicate {
  def check(x:Any): Boolean
}


class LEQ(n:Int) extends Predicate {
  def check(x: Any): Boolean =
    if (x.isInstanceOf[Int]) x.asInstanceOf[Int] <= n
    else false

  override def toString = "[<="+n+"]"
}
