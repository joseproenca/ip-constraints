package common.beh.guardedcommands.dataconnectors

import common.beh.guardedcommands._
import common.beh.Utils._
import common.beh.Predicate


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 12/11/12
 * Time: 16:52
 * To change this template use File | Settings | File Templates.
 */
class GCSimpleVarFilter(an : String, bn : String, pred: Predicate, uid: Int)
  extends GCConnector(List(an,bn),uid) {

  val a = mkVar(an,uid)
  val b = mkVar(bn,uid)

  var data: Option[Any] = None

  def getConstraints =
    if (!data.isDefined)  GuardedCommands(
      a --> (a :< pred),
      !b
    )
    else GuardedCommands(
      a --> (a :< pred),
      b --> (b := data.get)
    )

  override def update(sol: GCSolution) {
    if (sol hasFlow a)
      data = Some(sol dataOn a)
  }
}
