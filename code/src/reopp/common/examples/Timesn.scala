package reopp.common.examples

import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import z3.scala.{Z3AST, Z3Context}
import reopp.common.IntFunction

/** Example of an IntFunction. */
class Timesn(n: Int) extends IntFunction {
   val choFun = (x:IntegerExpressionVariable) => Choco.mult(x,n)
   val funFun = n * (_:Int)
   val z3Fun = (z:Z3Context,t:List[Z3AST]) => z.mkMul(z.mkInt(n,z.mkIntSort()),t.head)
   override def toString = "[*"+n+"]"
 }
