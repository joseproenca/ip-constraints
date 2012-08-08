package common.beh.guardedcommands.dataconnectors

import common.beh.guardedcommands._
import common.beh.Utils.{flowVar,st2GC,srcVar,snkVar}
import common.beh.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:23
 * To change this template use File | Settings | File Templates.
 */

class GCADrain (a: String, b: String, uid: Int) extends GCBehaviour(List(a,b), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))
  val asr = Var(srcVar(a,uid))
  val bsk = Var(snkVar(b,uid))

  var constraints = GuardedCommands(!(av and bv))

  if (useCC3)
    constraints ++= Set(
      av --> bsk,
      bv --> asr,
      (!av /\ !bv) --> (!asr /\ !bsk)
    )
}
