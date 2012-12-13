package common.choco.dataconnectors

import common.Utils
import Utils._
import common.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:06
 * To change this template use File | Settings | File Templates.
 */

class ChoExRouter(x:String,y:String,z:String,uid:Int) extends connectors.ChoExRouter(x,y,z,uid) {

  useData = true
  useCC3 = false

  override def getConstraints: ChoConstraints = {
    val c = super.getConstraints
    c impose List(
      yv --> VarEq(dataVar(x,uid),dataVar(y,uid)),
      zv --> VarEq(dataVar(x,uid),dataVar(z,uid))
    )
    c
  }

  // suggests which ends must have dataflow if "end" has also dataflow
//  def guessRequirements(end: String) = end match {
//    case `x` => Set(y) // priority to y
//    case `y` => Set(x)
//    case `z` => Set(x)
//    case _ => Set()
//  }
}
