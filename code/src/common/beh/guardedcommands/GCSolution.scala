package common.beh.guardedcommands

import common.beh.Solution
import common.beh.Utils.ppVar

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:58
 * To change this template use File | Settings | File Templates.
 */

class GCSolution(val boolSol: Solution, var varMap: Map[String, Int]) extends Solution {
  def hasFlow(end: String) =
    boolSol hasFlow end
  //if (boolSol contains end) varMap(end) else false

  def pretty: String = {
    var res = boolSol.pretty
    for ((v:String,k:Int) <- varMap.toList.sortBy((x:(String,Int)) => x))
      res += ppVar(v) + " -> "+k+"\n"
    res
  }

  def apply(v:String): Int = //varMap(v)
    if (varMap contains v) varMap(v)
    else if (boolSol hasFlow v) return 1
    else return 0
  def update(v:String,b:Int) {
    varMap = varMap + (v -> b)
  }

}
