package common.guardedcommands.dataconnectors

import common.guardedcommands.GCSolution
import common.{Function, Utils}
import Utils.dataVar

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 14/11/12
 * Time: 11:41
 * To change this template use File | Settings | File Templates.
 */
class GCTransfUndo (a: String, b: String, uid: Int, f: common.Function, undo: common.Function) extends GCTransf(a,b,uid,f) {

  override def update(s: Option[GCSolution]) {
    super.update(s)
    if (s.isDefined)
      if (s.get.buf.isDefined) {
//        println("trying to rollback")
        s.get.buf.get.rollback(f,undo,s.get getDataOn dataVar(a,uid))
      }
    else {
      println("===== buffer not defined!!!!")
    }
  }
}