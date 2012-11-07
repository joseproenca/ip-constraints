package common.beh.guardedcommands

import common.beh.Connector
import common.beh.Utils._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/11/12
 * Time: 16:31
 * To change this template use File | Settings | File Templates.
 */
class ComplexConnector(val sub: List[Connector[GCSolution,GuardedCommands]], ends: List[String], uid: Int = 0)
    extends GCConnector(ends, uid) {

  /**
   * Collect the constraints and returns them, ready to be solved.
   * Add sync rules: assume share variables for synchronisation,
   *   and connect CC3-related variables (assuming all ends use the same uid!)
   */
  def getConstraints = {
    var res = GuardedCommands()
    for (c <- sub) res ++= c.getConstraints
    if (useCC3)
      for (e <- ends)
        res ++= GuardedCommands( Var(srcVar(e,uid)) \/ Var(snkVar(e,uid)) )
    res
  }

  override def update(s: GCSolution) {
    for (c <- sub) c.update(s)
  }

  def +++(other: Connector[GCSolution,GuardedCommands]): ComplexConnector = other match {
    case c: ComplexConnector => new ComplexConnector(sub ++ c.sub,ends ++ c.ends, uid)
    case _ => new ComplexConnector(other :: sub, ends ++ other.ends, uid)
  }

}
