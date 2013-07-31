package reopp.common.choco

import reopp.common.Utils

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:04
 * To change this template use File | Settings | File Templates.
 */

abstract class ChoDataConnector(ends:List[String],uid:Int) extends ChoConnector(ends,uid) {

//  override def sync(from:AnyRef,c:ChoConstraints) = {
//    val c2 = super.sync(from,c)
//    if (connections contains from) {
//      val dataConstr = for ((end,oend,ouid) <- connections(from))
//        yield VarEq(Utils.dataVar(oend,ouid),Utils.dataVar(end,uid))
//      c2 ++ dataConstr
//    }
//    else c2
//  }
}
