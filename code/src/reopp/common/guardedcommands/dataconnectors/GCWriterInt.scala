package reopp.common.guardedcommands.dataconnectors

import reopp.common.{OptionSol, Utils}
import Utils._
import reopp.common.guardedcommands._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 17:29
 * To change this template use File | Settings | File Templates.
 */

@deprecated
class GCWriterInt(val x: String, uid: Int, var data: List[Int]) extends GCConnector(List(x), uid) {

  if (useCC3) throw new Exception("CC3 not implemented")
  
  def getConstraints: Formula = {
    if (!data.isEmpty) {
      if(useData) x := data.head  //Formula(xv --> IntAssgn(dataVar(x,uid),data.head)) //(xv := data.head))
      else Formula()
    }
    else !x
  }

  override def update(s: OptionSol[GCSolution]) {
    if (s.isDefined)
    if (s.get hasFlowOn mkVar(x)) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
//      constraints = loadConstraints
      data = data.tail
    }
  }

  override def isProactive: Boolean = !data.isEmpty

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

}
