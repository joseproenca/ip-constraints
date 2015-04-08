package reopp.common.examples

import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import z3.scala.{Z3AST, Z3Context}
import reopp.common.IntPredicate

/** Example of an [[reopp.common.IntPredicate]]. */
class Even extends IntPredicate {
   val choPred = (x: IntegerExpressionVariable) => Choco.eq(Choco.mod(x, 2), 0)
   val z3Pred = (z:Z3Context,v:Z3AST) => z.mkEq(z.mkMod(v,z.mkInt(2,z.mkIntSort())),z.mkInt(0,z.mkIntSort()))
   val funPred = (x: Int) => x % 2 == 0

   override def toString = "Even"
 }
