package common.beh.guardedcommands.dataconnectors

import common.beh.guardedcommands._
import common.beh.Utils._
import common.beh.guardedcommands.Var
import common.beh.Function

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/07/12
 * Time: 15:52
 * To change this template use File | Settings | File Templates.
 */

class GCMonitor (a: String, b: String, uid: Int, f: Function) extends GCSync(a,b,uid) {

  override def update(s: GCSolution) {
    super.update(s)
    if (s hasFlowOn av)
      f.calculate(s getDataOn av)
  }
}
