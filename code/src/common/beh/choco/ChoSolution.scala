package common.beh.choco

import choco.kernel.model.variables.integer.IntegerVariable
import choco.cp.solver.CPSolver
import scala.collection.mutable.{Map => MuMap}

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:11
 * To change this template use File | Settings | File Templates.
 */

class ChoSolution(s: CPSolver, varMap: Map[String, IntegerVariable]) extends common.beh.Solution {
  val extension = MuMap[String, Int]()
  
  def getVal(v: String): Option[Int] = { 
    if (varMap contains v)
      if (s contains varMap(v))
        return Some(s.getVar(varMap(v)).getVal)
    extension.get(v)
  }
  
  def hasFlow(v: String) = getVal(v) != 0

  def extend(v:String,i:Int) { extension(v) = i }
  
  def size = varMap.size + extension.size
  def sizeModel = s.getModel.getNbIntVars

  def pretty: String = {
    //        for (IntegerVariable c:
    var res: String = ""
//    val it: java.util.Iterator[IntegerVariable] = s.getModel.getIntVarIterator
//    while (it.hasNext) {
//      val variab: IntegerVariable = it.next
//      res += variab.getName + " -> " + s.getVar(variab).getVal + "\n";
//    }
//    res += "-\n"
    for ((k,v) <- varMap)
//      res += k +" '"+v + "'\n"
      res += k + " -> " + s.getVar(v).getVal + "\n"

    for (ex <- extension)
      res += ex._1 + " -> " + ex._2 + "\n"
    res
  }
}
