package dreams.connectors

import common.guardedcommands.{GuardedCommands, GCSolution, GCConnector}
import common._
import dreams.Actor
import common.{Solution, CBuilder, Constraints, Connector}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 12/11/12
 * Time: 16:24
 * To change this template use File | Settings | File Templates.
 */
object Channel {
  def apply[S<:Solution, C<:Constraints[S,C]]
      (conn : Int => Connector[S,C])
      (implicit noSol:EmptySol[S], b:CBuilder[S,C]): Actor[S,C] =
    new Actor[S,C]() {
//      val uid = this.hashCode()
      val behaviour = conn(uid)
    }
}