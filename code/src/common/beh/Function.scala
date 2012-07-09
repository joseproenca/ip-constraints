package common.beh

import _root_.choco.Choco
import _root_.choco.kernel.model.variables.integer.IntegerExpressionVariable

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 04/07/12
 * Time: 18:12
 * To change this template use File | Settings | File Templates.
 */

abstract class Function {
  val choFun: IntegerExpressionVariable => IntegerExpressionVariable
  val funFun: Int => Int
}

class Double extends Function {
  val choFun = (x:IntegerExpressionVariable) => Choco.mult(x,2)
  val funFun = 2 * (_:Int)
  override def toString() = "[*2]"
}

class Timesn(n: Int) extends Function {
  val choFun = (x:IntegerExpressionVariable) => Choco.mult(x,n)
  val funFun = n * (_:Int)
  override def toString() = "[*"+n+"]"
}
