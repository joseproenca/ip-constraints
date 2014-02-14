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
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))

  val emptyFifo = Formula(!bv)
//  def fullFifo = Formula(Set(
//    av --> SGuard(bv),
//    bv --> DataAssgn(dataVar(b,uid),data.get)
//  ))

  def fullFifo =
    if (useData) Formula(
      av --> bv,
      bv --> (bv := data.get)  // DataAssgn(dataVar(b,uid),data.get)
    )
    else if (useCC3) throw new Exception("CC3 not implemented")
    else Formula(av --> bv)


  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  // update state not implemented
}
