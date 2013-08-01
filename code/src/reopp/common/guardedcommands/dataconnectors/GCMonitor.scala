package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.{OptionSol, Function, Utils}
import Utils._
import reopp.common.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/07/12
 * Time: 15:52
 * To change this template use File | Settings | File Templates.
 */

class GCMonitor (a: String, b: String, uid: Int, f: Function) extends GCSync(a,b,uid) {

  override def update(s: OptionSol[GCSolution]) {
//    println("updating! - based on "+a+"\n"+s.get)
//    println("s.get of "+a+": "+s.get.getDataOn(dataVar(a,uid)))
    super.update(s)
    if (s.isDefined && (s.get hasFlowOn av) && s.get.getDataOn(dataVar(a, uid)).isDefined)
        f.calculate(s.get.getDataOn(dataVar(a,uid)).get)
  }
}
