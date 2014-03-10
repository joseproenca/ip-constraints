package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands.{GCConnector, GCSolution}
import reopp.common.{OptionSol, Function, Utils}
import Utils.{mkDataVar,mkFlowVar}
import reopp.common

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 14/11/12
 * Time: 11:41
 * To change this template use File | Settings | File Templates.
 */
class GCTransfUndo (a: String, b: String, f: common.Function, undo: common.Function)
    extends GCConnector(List(a,b)) {

  val newF = new Function { // wrapping function to guarantee its uniqueness (for a correct compensation)
    override def calculate(x:Any) = f.calculate(x)
    override def toString = f.toString
  }

  val transf = new GCTransf(a,b,newF)

  def getConstraints = transf.getConstraints

  override def update(s: OptionSol[GCSolution]) {
    super.update(s)
//    if (s.isDefined) { // && s.get.hasFlowOn(flowVar(a,uid))) {
      if (s.getBuffer.isDefined) {
//        println("trying to rollback")
        if (s.isDefined)
          s.getBuffer.get.rollback(newF,undo,s.get getDataOn mkDataVar(a))
        else
          s.getBuffer.get.rollback(newF,undo,None)
      }
      else {
        println("===== buffer not defined - required for compensation of transaction.")
      }
  }
}