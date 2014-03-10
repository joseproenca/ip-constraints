package reopp.common.guardedcommands

import reopp.common.{OptionSol, Connector, Utils}
import Utils._
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/11/12
 * Time: 16:31
 * To change this template use File | Settings | File Templates.
 */
class ComplexConnector(val sub: List[Connector[GCSolution,Formula]], ends: List[String])
    extends GCConnector(ends) {

  /**
   * Collect the constraints and returns them, ready to be solved.
   * Add sync rules: assume shared variables for synchronisation,
   *   and connect CC3-related variables (assuming their original ids - good!)
   */
  def getConstraints = {
    var res = Formula()
    for (c <- sub) res ++= c.getConstraints
    if (useCC3) {
    	val subends = mutable.Set[String]()
      for (c <- sub; e <- c.ends)
        subends add e
      for (e <- subends)
        res ++= Formula( Var(toSrcVar(e)) \/ Var(toSnkVar(e)) )
    }
    res
  }

//  override def updateID(newID:Int) {
//    super.updateID(newID)
//    sub.map(_.updateID(newID))
//  }

  override def update(s: OptionSol[GCSolution]) {
    for (c <- sub) c.update(s)
  }

  def +++(other: Connector[GCSolution,Formula]): ComplexConnector = other match {
    // Note: could drop repeated names, if needed.
    case c: ComplexConnector => new ComplexConnector(sub ++ c.sub,ends ++ c.ends)
    case _ => new ComplexConnector(other :: sub, ends ++ other.ends)
  }

  override def isProactive: Boolean = {
    for (c <- sub) if (c.isProactive) return true
    false
  }

}
