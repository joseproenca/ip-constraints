package common.examples

import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import z3.scala.{Z3AST, Z3Context}
import common.IntFunction

/** Example of an IntFunction  */
class Double extends IntFunction {
   val choFun = (x:IntegerExpressionVariable) => Choco.mult(x,2)
   val funFun = 2 * (_:Int)
   val z3Fun = (z:Z3Context,t:List[Z3AST]) => z.mkMul(z.mkInt(2,z.mkIntSort()),t.head)
   override def toString = "[*2]"
 }
