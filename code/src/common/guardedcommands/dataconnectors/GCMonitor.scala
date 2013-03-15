package common.guardedcommands.dataconnectors

import common.guardedcommands._
import common.{Function, Utils}
import Utils._
import common.guardedcommands.Var

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
      f.calculate(s(av))
  }
}
