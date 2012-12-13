package common.guardedcommands

import common.{Utils, Solution}
import common.EmptySol

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/06/12
 * Time: 12:37
 * To change this template use File | Settings | File Templates.
 */

class GCBoolSolution(var varMap: Map[String, Boolean]) extends Solution {
  def hasFlowOn(end: String) =
    if (varMap contains end) varMap(end) else false

  override def toString: String = {
    var res = ""
    for ((v:String,k:Boolean) <- varMap.toList.sortBy((x:(String,Boolean)) => x))
      res += common.Utils.ppVar(v) + " -> "+k+"\n"
    res
  }

  def getDataOn(x:String) = None

  def apply(v:String) = //varMap(v)
    //if (varMap contains v) varMap(v) else false // if it is not mentioned, probably it was not relevant. Use default "false"
    hasFlowOn(v)
  def update(v:String,b:Boolean) {
    varMap = varMap + (v -> b)
  }

}

object GCBoolSolution {
  implicit object NoSol extends EmptySol[GCBoolSolution] {
    def sol = new GCBoolSolution(Map())
  }
}

