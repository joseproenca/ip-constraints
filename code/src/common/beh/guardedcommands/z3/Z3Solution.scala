package common.beh.guardedcommands.z3

import common.beh.Solution
import z3.scala.{Z3Model, Z3Context}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 31/07/12
 * Time: 15:03
 * To change this template use File | Settings | File Templates.
 */

class Z3Solution(z3: Z3Context, model: Z3Model) extends Solution {
  def hasFlow(end: String) = model.evalAs[Boolean](z3.mkBoolConst(z3.mkStringSymbol(end))) match {
    case None => false
    case Some(b) => b
  }

  def pretty = model.toString()
}
