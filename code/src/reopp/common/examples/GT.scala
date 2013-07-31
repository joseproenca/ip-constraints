package reopp.common.examples

import choco.Choco
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import z3.scala.{Z3AST, Z3Context}
import reopp.common.IntPredicate

/** Example of an [[common.IntPredicate]]. */
class GT(i: Int) extends IntPredicate {
   val choPred = Choco.gt(_: IntegerExpressionVariable, i)
   val z3Pred = (z:Z3Context,v:Z3AST) => z.mkGT(v,z.mkInt(i,z.mkIntSort()))
   val funPred = i < (_: Int)

   override def toString = "[>" + i + "]"
 }
