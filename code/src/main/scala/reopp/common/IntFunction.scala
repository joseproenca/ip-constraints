package reopp.common

import _root_.choco.Choco
import _root_.choco.kernel.model.variables.integer.IntegerExpressionVariable
import z3.scala.{Z3AST, Z3Context}
import scala.Predef._

/**
 * Special [[reopp.common.Function]] over integers that has associated constraints
 * that can be integrated with Choco and Z3.
 *
 * Created by jose on 04/07/12.
 */
abstract class IntFunction extends reopp.common.Function {
  val choFun: IntegerExpressionVariable => IntegerExpressionVariable
  val z3Fun: (Z3Context,List[Z3AST]) => Z3AST
  val funFun: Int => Int

  def calculate(x:Any): Any = {
    if (x.isInstanceOf[Int]) funFun(x.asInstanceOf[Int])
    else throw new RuntimeException(this + " is only defined for Int - failed to process "+x)
  }
}




