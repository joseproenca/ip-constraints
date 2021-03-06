package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.{OptionSol, Predicate, Utils}
import Utils._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 12/11/12
 * Time: 16:52
 * To change this template use File | Settings | File Templates.
 */
class GCSimpleVarFilter(a : String, b : String, pred: Predicate)
  extends GCConnector(List(a,b)) {

  var data: Option[AnyRef] = None

  def getConstraints =
    if (!data.isDefined)  Formula(
      a --> (a :< pred),
      !b
    )
    else Formula(
      a --> (a :< pred),
      b --> (b :== data.get)
    )

  override def update(sol: OptionSol[GCSolution]) {
    if (sol.isDefined)
      if (sol.get hasFlowOn a.flow)
        data = Some(sol.get getDataOn a.data)
  }
}
