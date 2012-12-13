package common

import _root_.choco.kernel.model.constraints.Constraint
import _root_.choco.kernel.model.variables.integer.IntegerExpressionVariable
import _root_.choco.Choco
import z3.scala.{Z3Context, Z3AST}

/**
 * Special [[common.Predicate]] over integers that has associated constraints
 * that can be integrated with Choco and Z3.
 *
 * Created by jose on 06/06/12.
 */
abstract class IntPredicate extends Predicate {
  val choPred: IntegerExpressionVariable => Constraint
  val z3Pred: (Z3Context,Z3AST) => Z3AST
  val funPred: Int => Boolean

  def check(x: Any): Boolean =
    if (x.isInstanceOf[Int]) funPred(x.asInstanceOf[Int])
    else false
}








