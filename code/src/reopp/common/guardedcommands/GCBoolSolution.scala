package reopp.common.guardedcommands

import reopp.common.{Buffer, Utils, Solution, EmptySol}
import reopp.common.Utils.addID

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/06/12
 * Time: 12:37
 * To change this template use File | Settings | File Templates.
 */

class GCBoolSolution(var varMap: Map[String, Boolean], buf: Option[Buffer]) extends Solution[GCBoolSolution] {
  override def getBuffer = buf

  def hasFlowOn(end: String) =
    if (varMap contains end) varMap(end) else false

  def getDataOn(x:String) = None

  type S = GCBoolSolution
  def withID(id:Int) = new GCBoolSolution(varMap,buf) {
    override def hasFlowOn(end:String) = super.hasFlowOn(addID(end,id))
    override def getDataOn(end:String) = super.getDataOn(addID(end,id))
  }
        
  override def toString: String = {
    var res = ""
    for ((v:String,k:Boolean) <- varMap.toList.sortBy((x:(String,Boolean)) => x))
      res += reopp.common.Utils.ppVar(v) + " -> "+k+"\n"
    res
  }

  def apply(v:String) = //varMap(v)
    //if (varMap contains v) varMap(v) else false // if it is not mentioned, probably it was not relevant. Use default "false"
    hasFlowOn(v)
  def update(v:String,b:Boolean) {
    varMap = varMap + (v -> b)
  }

}

object GCBoolSolution {
  implicit object NoSol extends EmptySol[GCBoolSolution] {
    def sol = new GCBoolSolution(Map(),None)
  }
}

