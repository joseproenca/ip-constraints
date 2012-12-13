package common.examples

import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import z3.scala.{Z3AST, Z3Context}
import common.IntFunction

/** Example of an IntFunction. */
class Timesn(n: Int) extends IntFunction {
   val choFun = (x:IntegerExpressionVariable) => Choco.mult(x,n)
   val funFun = n * (_:Int)
   val z3Fun = (z:Z3Context,t:Z3AST) => z.mkMul(z.mkInt(n,z.mkIntSort()),t)
   override def toString = "[*"+n+"]"
 }
