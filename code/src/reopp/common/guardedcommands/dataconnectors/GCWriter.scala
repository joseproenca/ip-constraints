package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.{OptionSol, Utils}
import Utils._
import reopp.common.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:15
 * To change this template use File | Settings | File Templates.
 */

class GCWriter (val x: String, uid: Int, var data: List[Any]) extends GCConnector(List(x), uid) {

//  def this(x: String, uid: Int, dt: List[Int]) = this(x, uid, dt.map(Int.box(_)))
  def this(x: String, uid: Int) = this(x, uid, Nil: List[Any])


  //var constraints = loadConstraints

  def getConstraints = {
    if (!data.isEmpty) {
      if(useData)
        x --> (x :== data.head)//(Var(flowVar(x,uid)) --> DataAssgn(dataVar(x,uid),data.head))

      else if (useCC3) throw new Exception("CC3 not implemented")
      else Formula()
    }
    else !x
  }

  override def update(s: OptionSol[GCSolution]) {
    if (s.isDefined)
    if (s.get.hasFlowOn(mkVar(x))) {
      //      println("Writer: FLOW! new size: "+size)
      notifyflow()
      // update state
      data = data.tail
      // update constraints
      // constraints = loadConstraints --- done by getConstraints by updating state
    }
  }

  override def isProactive: Boolean = !data.isEmpty

  // suggests which ends must have dataflow if "end" has also dataflow
  def guessRequirements(end: String) = Set()

}

