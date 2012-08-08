package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:43
 * To change this template use File | Settings | File Templates.
 */

class GCFifo(a: String, b: String, var data: Option[Any], uid: Int) extends GCBehaviour(List(a,b), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))

  val emptyFifo = GuardedCommands(!bv)

  def fullFifo =
    if (useData) GuardedCommands(
        !av,
        bv --> (bv := data.get)
      )
    else if (useCC3) throw new Exception("CC3 not implemented")
    else GuardedCommands(Neg(av))


  def loadConstraints = if (data.isDefined) fullFifo else emptyFifo

  var constraints = loadConstraints

  // update state not implemented
}
