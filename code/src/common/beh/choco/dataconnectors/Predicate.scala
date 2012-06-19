package common.beh.choco.dataconnectors

import choco.kernel.model.variables.integer.IntegerVariable
import choco.kernel.model.constraints.Constraint
import choco.Choco

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 15:39
 * To change this template use File | Settings | File Templates.
 */

abstract class Predicate {
  val choPred: IntegerVariable => Constraint
  val funPred: Int => Boolean
}

class GT(i:Int) extends Predicate {
  val choPred = Choco.gt(_:IntegerVariable,i)
  val funPred = i < (_:Int)
  override def toString = "[>"+i+"]"
}

class LT(i:Int) extends Predicate {
  val choPred = Choco.lt(_:IntegerVariable,i)
  val funPred = i > (_:Int)
  override def toString = "[<"+i+"]"
}

class Even extends Predicate {
  val choPred = (x:IntegerVariable) => Choco.eq(Choco.mod(x,2),0)
  val funPred = (x:Int) => x % 2 == 0
  override def toString = "Even"
}

class Odd extends Predicate {
  val choPred = (x:IntegerVariable) => Choco.eq(Choco.mod(x,2),1)
  val funPred = (x:Int) => x % 2 == 1
  override def toString = "Odd"
}
