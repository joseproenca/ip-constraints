package reopp.common.choco.dataconnectors

import reopp.common.choco._
import reopp.common.Utils
import Utils._


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 10:50
 * To change this template use File | Settings | File Templates.
 */

class ChoLossy(x: String, y: String, uid: Int) extends connectors.ChoLossy(x, y, uid) {

  useData = true
  useCC3 = false

  override def getConstraints: ChoConstraints = {
    val c = super.getConstraints
    c impose ( VarEq(dataVar(x,uid),dataVar(y,uid)) )
    c
  }
//  constraints impose (
//    VarEq(dataVar(x,uid),dataVar(y,uid))
//  )

  // suggests which ends must have dataflow if "end" has also dataflow
  //  def guessRequirements(end: String) = if (end == x) Set(y) else Set(x)
}
