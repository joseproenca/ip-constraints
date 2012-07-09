package common.beh.choco.connectors

import common.beh.Utils._
import common.beh.choco.{Neg, ChoConstraints, Var, ChoBehaviour}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:03
 * To change this template use File | Settings | File Templates.
 */

class ChoExRouter(x:String,y:String,z:String,uid:Int) extends ChoBehaviour(List(x,y),uid) {

  useData = false
  useCC3 = false

  val (xv,yv,zv) = (Var(flowVar(x,uid)) , Var(flowVar(y,uid)), Var(flowVar(z,uid)))

  var constraints = ChoConstraints(List((xv <-> (yv or zv)) , Neg(yv and zv)))

  // suggests which ends must have dataflow if "end" has also dataflow
//  def guessRequirements(end: String) = end match {
//    case `x` => Set(y) // priority to y
//    case `y` => Set(x)
//    case `z` => Set(x)
//    case _ => Set()
//  }
}
