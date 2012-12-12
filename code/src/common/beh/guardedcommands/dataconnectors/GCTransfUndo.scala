package common.beh.guardedcommands.dataconnectors

import common.beh.Function
import common.beh.guardedcommands.GCSolution
import common.beh.Utils.dataVar

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 14/11/12
 * Time: 11:41
 * To change this template use File | Settings | File Templates.
 */
class GCTransfUndo (a: String, b: String, uid: Int, f: Function, undo: Function) extends GCTransf(a,b,uid,f) {

  override def update(s: GCSolution) {
    super.update(s)
    if (s.buf.isDefined)
      s.buf.get.rollback(f,undo,s getDataOn dataVar(a,uid))
//    else {
//      println("===== buffer not defined!!!!")
//    }
  }
}