package common.beh.choco.dataconnectors

import common.beh.Utils._
import common.beh.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:13
 * To change this template use File | Settings | File Templates.
 */

class ChoMerger(x:String,y:String,z:String,uid:Int) extends ChoDataBehaviour(List(x,y),uid) {
  val (xv,yv,zv) = (Var(flowVar(x,uid)) , Var(flowVar(y,uid)), Var(flowVar(z,uid)))

  var constraints = ChoConstraints(List(
    (zv <-> (xv or yv)),
    Neg(xv and yv),
    xv --> VarEq(dataVar(x,uid),dataVar(z,uid)),
    yv --> VarEq(dataVar(y,uid),dataVar(z,uid))
  ))

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = end match {
    case `x` => Set(z)
    case `y` => Set(z)
    case `z` => Set(x) // priority to x
    case _ => Set()
  }
}