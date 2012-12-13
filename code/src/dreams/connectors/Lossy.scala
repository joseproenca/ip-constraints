package dreams.connectors

import dreams.Actor
import common.guardedcommands.{ GCSolution, GuardedCommands}
import common.guardedcommands.dataconnectors.{GCLossy}
import common.guardedcommands.GCConnector.GCBuilder // provides implicit builders

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 03/05/12
 * Time: 10:37
 * To change this template use File | Settings | File Templates.
 */

class Lossy extends Actor[GCSolution, GuardedCommands] {
//  private val uid = hashCode

  val behaviour = new GCLossy("x","y",uid)

//// Also works, but this should be done in choco.connectors.
//  val constr = ChoConstraints(Impl(Var(ConstrBuilder.flowVar("a",uid)), Var(ConstrBuilder.flowVar("b",uid))))
//
//  val behaviour: ChoBehaviour = ChoBehaviour(List("a"),uid,constr) // or new ChoLossy("a","b",uid)
}

