package reopp.common.guardedcommands

import reopp.common._
import reopp.common.Utils.{ppVar,mkDataVar,addID}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:58
 * To change this template use File | Settings | File Templates.
 */

class GCSolution(val boolSol: Solution[_], var varMap: Map[String, Any]) extends Solution[GCSolution] {
//  var buf: Option[Buffer] = None
  override def getBuffer = boolSol.getBuffer //buf

  def hasFlowOnPort(end: Var): Boolean = hasFlowOn(end.name)
  
  def hasFlowOn(end: String) =
    boolSol hasFlowOn end
  //if (boolSol contains end) varMap(end) else false

  type S = GCSolution
  def withID(id:Int) = new GCSolution(boolSol,varMap) {
	    override def hasFlowOn(end:String) = super.hasFlowOn(addID(end,id))
	    override def getDataOn(end:String) = super.getDataOn(addID(end,id))
  }
 
    
  override def toString: String = {
    var res = boolSol.toString
    for ((v:String,k:Any) <- varMap.toList.sortBy((x:(String,Any)) => x._1))
      res += ppVar(v) + " -> "+k+"\n"
    res
  }

  def getDataOn(end: String) = varMap.get(end)

//  def getDataOn(end: Var) = varMap.get(dataVar(end.name))


//  def apply(end: Var): Any = apply(dataVar(end.name))
//
  // not needed, but used in tests (laziness)
  def apply(v:String): Any = //varMap(v)
    if (varMap contains v) varMap(v)
    else boolSol hasFlowOn v
//
//  def update(v:String,b:Int) {
//    varMap = varMap + (v -> b)
//  }

}

object GCSolution {
  class MyEmptySol extends Solution[MyEmptySol] {
    def hasFlowOn(end: String) = false
    def getDataOn(end: String) = None
    type S = MyEmptySol
    def withID(id:Int) = this
    override def toString = "Ø"
  }

  implicit object NoSol extends EmptySol[GCSolution] {
    def sol = {
      new GCSolution(new MyEmptySol,Map())
    }
  }
}
