package common.beh

import _root_.choco.Choco
import _root_.choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.genericconstraints.UnFunction

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 04/07/12
 * Time: 18:12
 * To change this template use File | Settings | File Templates.
 */

abstract class IntFunction extends UnFunction {
  val choFun: IntegerExpressionVariable => IntegerExpressionVariable
  val funFun: Int => Int

  def calculate(x:Any): Any = {
    if (x.isInstanceOf[Int]) funFun(x.asInstanceOf[Int])
    else throw new RuntimeException(this + " is only defined for Int - failed to process "+x)
  }
}

class Double extends IntFunction {
  val choFun = (x:IntegerExpressionVariable) => Choco.mult(x,2)
  val funFun = 2 * (_:Int)
  override def toString() = "[*2]"
}

class Timesn(n: Int) extends IntFunction {
  val choFun = (x:IntegerExpressionVariable) => Choco.mult(x,n)
  val funFun = n * (_:Int)
  override def toString() = "[*"+n+"]"
}
