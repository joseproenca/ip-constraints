package common.beh

import _root_.choco.kernel.model.constraints.Constraint
import _root_.choco.kernel.model.variables.integer.IntegerExpressionVariable
import _root_.choco.Choco
import choco.genericconstraints.UnPredicate

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 15:39
 * To change this template use File | Settings | File Templates.
 */

abstract class IntPredicate extends UnPredicate {
  val choPred: IntegerExpressionVariable => Constraint
  val funPred: Int => Boolean

  def check(x: Any): Boolean =
    if (x.isInstanceOf[Int]) funPred(x.asInstanceOf[Int])
    else false
}

class GT(i: Int) extends IntPredicate {
  val choPred = Choco.gt(_: IntegerExpressionVariable, i)
  val funPred = i < (_: Int)

  override def toString = "[>" + i + "]"
}

class LT(i: Int) extends IntPredicate {
  val choPred = Choco.lt(_: IntegerExpressionVariable, i)
  val funPred = i > (_: Int)

  override def toString = "[<" + i + "]"
}

class Even extends IntPredicate {
  val choPred = (x: IntegerExpressionVariable) => Choco.eq(Choco.mod(x, 2), 0)
  val funPred = (x: Int) => x % 2 == 0

  override def toString = "Even"
}

class Odd extends IntPredicate {
  val choPred = (x: IntegerExpressionVariable) => Choco.eq(Choco.mod(x, 2), 1)
  val funPred = (x: Int) => x % 2 == 1

  override def toString = "Odd"
}
