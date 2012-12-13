package common.choco.dataconnectors

import common.Utils
import Utils._
import common.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:15
 * To change this template use File | Settings | File Templates.
 */

class ChoSync(a:String,b:String,uid:Int) extends connectors.ChoSync(a,b,uid) {

  useData = true
  useCC3 = false


  override def getConstraints: ChoConstraints = {
    val c = super.getConstraints
    c impose List(
      VarEq(flowVar(a,uid),flowVar(b,uid)),
      Var(flowVar(a,uid)) --> VarEq(dataVar(a,uid),dataVar(b,uid))
    )
    c
  }

}

