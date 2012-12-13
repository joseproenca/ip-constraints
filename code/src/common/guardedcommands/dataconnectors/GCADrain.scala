package common.guardedcommands.dataconnectors

import common.guardedcommands._
import common.Utils
import Utils.{flowVar,st2GC,srcVar,snkVar,st2GCs}
import common.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:23
 * To change this template use File | Settings | File Templates.
 */

class GCADrain (a: String, b: String, uid: Int) extends GCConnector(List(a,b), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))
  val asr = Var(srcVar(a,uid))
  val bsk = Var(snkVar(b,uid))

  private var constraints: GuardedCommands = !(av and bv)

  if (useCC3)
    constraints ++= Set(
      av --> bsk,
      bv --> asr,
      (!av /\ !bv) --> (!asr /\ !bsk)
    )

  def getConstraints = constraints
}
