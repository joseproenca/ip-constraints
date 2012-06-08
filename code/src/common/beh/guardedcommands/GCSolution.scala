package common.beh.guardedcommands

import common.beh.Solution

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:58
 * To change this template use File | Settings | File Templates.
 */

class GCSolution(var varMap: Map[String, Boolean]) extends Solution {
  def hasFlow(end: String) =
    if (varMap contains end) varMap(end) else false

  def pretty: String = {
    var res = ""
    for ((v:String,k:Boolean) <- varMap.toList.sortBy((x:(String,Boolean)) => x))
      res += v + " -> "+k+"\n"
    res
  }

  def apply(v:String) = varMap(v)
  // if (varMap contains v) varMap(v) else false
  def update(v:String,b:Boolean) {
    varMap = varMap + (v -> b)
  }

}
