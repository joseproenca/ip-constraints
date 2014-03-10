package reopp.dreams.connectors

import reopp.dreams.Actor
import reopp.common.guardedcommands.{ GCSolution, Formula}
import reopp.common.guardedcommands.dataconnectors.{GCLossy}
import reopp.common.guardedcommands.GCConnector.GCBuilder // provides implicit builders

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 03/05/12
 * Time: 10:37
 * To change this template use File | Settings | File Templates.
 */

class Lossy extends Actor[GCSolution, Formula] {
//  private val uid = hashCode

  val behaviour = new GCLossy("x","y")

//// Also works, but this should be done in choco.connectors.
//  val constr = ChoConstraints(Impl(Var(ConstrBuilder.flowVar("a",uid)), Var(ConstrBuilder.flowVar("b",uid))))
//
//  val behaviour: ChoBehaviour = ChoBehaviour(List("a"),uid,constr) // or new ChoLossy("a","b",uid)
}

