package common.choco.connectors

import common.choco._
import common.Utils
import Utils._
import common.choco.Var


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 03/05/12
 * Time: 10:50
 * To change this template use File | Settings | File Templates.
 */

class ChoLossy(x:String,y:String,uid:Int) extends ChoConnector(List(x,y),uid) {

  useData = false
  useCC3 = false

  val (xv,yv) = (Var(flowVar(x,uid)) , Var(flowVar(y,uid)))

  //with GuessReqChannel[ChoSolution,ChoConstraints] {
//  var constraints = ChoConstraints(And(
//    Impl(Var(ConstrBuilder.flowVar(y,uid)), Var(ConstrBuilder.flowVar(x,uid))),
//    (Var(ConstrBuilder.flowVar(x,uid)))
//  ))
  def getConstraints = ChoConstraints(
    yv --> xv
  )

  // suggests which ends must have dataflow if "end" has also dataflow
//  def guessRequirements(end: String) = if (end == x) Set(y) else Set(x)
}
