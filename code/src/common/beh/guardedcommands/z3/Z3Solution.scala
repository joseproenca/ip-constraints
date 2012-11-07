package common.beh.guardedcommands.z3

import common.beh.{EmptySol, Solution}
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

  def dataOn(end: String) =
    model.evalAs[Int](z3.mkBoolConst(z3.mkStringSymbol(end)))

  def pretty = model.toString()
}

object GCSolution {
  implicit object NoSol extends EmptySol[Z3Solution] {
    def sol = new Z3Solution(null,null) {
      override def hasFlow(end: String) = false
      override def dataOn(end:String) = None
      override def pretty = ""
    }
  }
}

