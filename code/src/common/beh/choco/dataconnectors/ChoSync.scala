package common.beh.choco.dataconnectors

import common.beh.Utils._
import common.beh.choco._

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

  constraints impose List(
    VarEq(flowVar(a,uid),flowVar(b,uid)),
    Var(flowVar(a,uid)) --> VarEq(dataVar(a,uid),dataVar(b,uid))
  )

}

