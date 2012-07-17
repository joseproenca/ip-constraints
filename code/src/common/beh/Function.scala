package common.beh

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 10/07/12
 * Time: 17:31
 * To change this template use File | Settings | File Templates.
 */

@deprecated
abstract class Function {
  def calculate(x:Any): Any
}

class Times(n:Int) extends Function {
  def calculate(x: Any): Any =
    if (x.isInstanceOf[Int]) x.asInstanceOf[Int] * n
    else throw new RuntimeException(this + " is only defined for Int - failed to process "+x)

  override def toString = "[*"+n+"]"
}
