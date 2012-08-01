package common.beh

import _root_.choco.kernel.model.constraints.Constraint
import _root_.choco.kernel.model.variables.integer.IntegerExpressionVariable
import _root_.choco.Choco
import choco.genericconstraints.UnPredicate
import z3.scala.{Z3Context, Z3AST}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 15:39
 * To change this template use File | Settings | File Templates.
 */

abstract class IntPredicate extends UnPredicate {
  val choPred: IntegerExpressionVariable => Constraint
  val z3Pred: (Z3Context,Z3AST) => Z3AST
  val funPred: Int => Boolean

  def check(x: Any): Boolean =
    if (x.isInstanceOf[Int]) funPred(x.asInstanceOf[Int])
    else false
}

class GT(i: Int) extends IntPredicate {
  val choPred = Choco.gt(_: IntegerExpressionVariable, i)
  val z3Pred = (z:Z3Context,v:Z3AST) => z.mkGT(v,z.mkInt(i,z.mkIntSort()))
  val funPred = i < (_: Int)

  override def toString = "[>" + i + "]"
}

class LT(i: Int) extends IntPredicate {
  val choPred = Choco.lt(_: IntegerExpressionVariable, i)
  val z3Pred = (z:Z3Context,v:Z3AST) => z.mkLT(v,z.mkInt(i,z.mkIntSort()))
  val funPred = i > (_: Int)

  override def toString = "[<" + i + "]"
}

class Even extends IntPredicate {
  val choPred = (x: IntegerExpressionVariable) => Choco.eq(Choco.mod(x, 2), 0)
  val z3Pred = (z:Z3Context,v:Z3AST) => z.mkEq(z.mkMod(v,z.mkInt(2,z.mkIntSort())),z.mkInt(0,z.mkIntSort()))
  val funPred = (x: Int) => x % 2 == 0

  override def toString = "Even"
}

class Odd extends IntPredicate {
  val choPred = (x: IntegerExpressionVariable) => Choco.eq(Choco.mod(x, 2), 1)
  val z3Pred = (z:Z3Context,v:Z3AST) => z.mkEq(z.mkMod(v,z.mkInt(2,z.mkIntSort())),z.mkInt(1,z.mkIntSort()))
  val funPred = (x: Int) => x % 2 == 1

  override def toString = "Odd"
}
