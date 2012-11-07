package common.beh.choco.connectors

import common.beh.Utils._
import common.beh.choco._


//import common.beh.ConnectorBeh

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 30/04/12
 * Time: 13:56
 * To change this template use File | Settings | File Templates.
 */

class ChoMerger(x:String,y:String,z:String,uid:Int) extends ChoConnector(List(x,y),uid) {

  useData = false
  useCC3 = false

  val (xv,yv,zv) = (Var(flowVar(x,uid)) , Var(flowVar(y,uid)), Var(flowVar(z,uid)))

  //(z <-> (x \/ y))  /\ !(a/\b)
  def getConstraints = ChoConstraints(List(
    zv <-> (xv or yv),
    Neg(xv and yv)
  ))

  // suggests which ends must have dataflow if "end" has also dataflow
//  def guessRequirements(end: String) = end match {
//    case `x` => Set(z)
//    case `y` => Set(z)
//    case `z` => Set(x) // priority to x
//    case _ => Set()
//  }
}

