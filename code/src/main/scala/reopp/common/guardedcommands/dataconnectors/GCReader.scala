package reopp.common.guardedcommands.dataconnectors

import reopp.common.{OptionSol, Utils}
import Utils._
import reopp.common.guardedcommands._

/**
 * Creates a reader with that can receive up to "size" elements, or an infinite number if size=-1.
 * It also prints the value that it received, and notifies flow to listeners of the constraint.
 *
 * User: jose
 * Date: 07/06/12
 * Time: 17:34
 * To change this template use File | Settings | File Templates.
 */

class GCReader(x: String, var size: Int) extends GCConnector(List(x)) {

  def getConstraints =
    if (size != 0) Formula()
    else !x

  override def update(s: OptionSol[GCSolution]) {
    if (s.isDefined)
    if (s.get hasFlowOn mkVar(x)) {
      println("//////////////////")
      println("// Got data - "+x+": "+s.get.getDataOn(mkDataVar(x)))
//      println("// new size: "+size)
      println("//////////////////")
      notifyflow()
//      constraints = loadConstraints
      size -= 1
    }
  }

  override def isProactive: Boolean = size > 0

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

  if (useCC3) throw new Exception("CC3 not implemented")
}