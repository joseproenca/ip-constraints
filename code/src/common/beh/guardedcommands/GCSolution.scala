package common.beh.guardedcommands

import common.beh.{EmptySol, Solution}
import common.beh.Utils.ppVar
import common.beh.choco.genericconstraints.Buffer

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:58
 * To change this template use File | Settings | File Templates.
 */

class GCSolution(val boolSol: Solution, var varMap: Map[String, Any]) extends Solution {
  var buf: Option[Buffer] = None

  def hasFlowOn(end: String) =
    boolSol hasFlowOn end
  //if (boolSol contains end) varMap(end) else false

  def pretty: String = {
    var res = boolSol.pretty
    for ((v:String,k:Any) <- varMap.toList.sortBy((x:(String,Any)) => x._1))
      res += ppVar(v) + " -> "+k+"\n"
    res
  }

  def getDataOn(end: String) = varMap.get(end)

  def apply(v:String): Any = //varMap(v)
    if (varMap contains v) varMap(v)
    else boolSol hasFlowOn v

  def update(v:String,b:Int) {
    varMap = varMap + (v -> b)
  }

}

object GCSolution {
  class MyEmptySol extends Solution {
    def hasFlowOn(end: String) = false
    def getDataOn(end: String) = None
    def pretty = ""
  }

  implicit object NoSol extends EmptySol[GCSolution] {
    def sol = {
      new GCSolution(new MyEmptySol,Map())
    }
  }
}
