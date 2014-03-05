package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 15:45
 * To change this template use File | Settings | File Templates.
 */

class GCSyncFifo(a: String, b: String, var data: Option[Any], uid: Int) extends GCConnector(List(a,b), uid) {

  if (useCC3) throw new Exception("CC3 not implemented")

  def fullFifo =
    if (useData) Formula(
      a --> b,
      b --> (b :== data.get)  // DataAssgn(dataVar(b,uid),data.get)
    )
    else Formula(a --> b)

  def getConstraints = if (data.isDefined) fullFifo else !b

  // update state not implemented
}
