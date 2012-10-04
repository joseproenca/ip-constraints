package common.beh.choco.dataconnectors

import common.beh.choco._
import common.beh.Utils._
import common.beh.choco.Neg
import common.beh.choco.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 19/06/12
 * Time: 21:04
 * To change this template use File | Settings | File Templates.
 */

class ChoIMerger(x:String,y:String,z:String,uid:Int) extends ChoDataConnector(List(x,y),uid) {

  useData = true
  useCC3 = false

  val (xv,yv,zv) = (Var(flowVar(x,uid)) , Var(flowVar(y,uid)), Var(flowVar(z,uid)))

  var constraints = ChoConstraints(List(
    (zv <-> (xv or yv)),
    (xv and yv) --> VarEq(dataVar(x,uid),dataVar(z,uid)) or VarEq(dataVar(y,uid),dataVar(z,uid)),
    (Neg(xv) and yv)  --> VarEq(dataVar(y,uid),dataVar(z,uid)),
    (xv and Neg(yv))  --> VarEq(dataVar(x,uid),dataVar(z,uid))
  ))

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = end match {
    case `x` => Set(z)
    case `y` => Set(z)
    case `z` => Set(x) // priority to x
    case _ => Set()
  }
}