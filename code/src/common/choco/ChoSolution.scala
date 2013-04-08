package common.choco

import choco.kernel.model.variables.integer.IntegerVariable
import choco.cp.solver.CPSolver
import scala.collection.mutable.{Map => MuMap}
import common.{Solution, EmptySol}

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:11
 * To change this template use File | Settings | File Templates.
 */

class ChoSolution(val choSol: CPSolver, varMap: Map[String, IntegerVariable]) extends Solution {
  val extension = MuMap[String, Int]()


  def getDataOn(v: String) = getVal(v).map(Int.box(_))

  def getVal(v: String): Option[Int] = { 
    if (varMap contains v)
      if (choSol contains varMap(v))
        return Some(choSol.getVar(varMap(v)).getVal)
    extension.get(v)
  }
  
  def hasFlowOn(v: String) = {
//    println("has flow on '"+v+"'? - getVal: "+ getVal(v))
    getVal(v) == Some(1)
  }

  def sol2map: Map[String, Boolean] = {
    var res = Map[String,Boolean]()
    for ((k,v) <- varMap)
      res += k  -> (choSol.getVar(v).getVal == 1)
    for (ex <- extension)
      res += ex._1  ->  (ex._2 == 1)
    res
  }

  def extend(v:String,i:Int) { extension(v) = i }

  def size = varMap.size + extension.size
  def sizeModel = choSol.getModel.getNbIntVars

  override def toString: String = {
    var res: String = ""
//    val it: java.util.Iterator[IntegerVariable] = choSol.getModel.getIntVarIterator
//    while (it.hasNext) {
//      val variab: IntegerVariable = it.next
//      res += variab.getName + " -> " + choSol.getVar(variab).getVal + "\n";
//    }
//    res += "-\n"
    // ERROR if varMap has has IntegerVariables that are not in the solution....

//    for ((k:String,v:IntegerVariable) <- varMap.toList.sortBy((x:(String,IntegerVariable)) => x))
//          res += k + " -> " + choSol.getVar(v).getVal + "\n"


    for ((k,v) <- varMap.toList.sortBy((x:(String,IntegerVariable)) => x._1))
      if (choSol.contains(v))
        res += common.Utils.ppVar(k) + " -> " + choSol.getVar(v).getVal + "\n"
      else
        res += common.Utils.ppVar(k) + " -> NOFLOW\n"
    //      res += k +" '"+v + "'\n"

    for (ex <- extension)
      res += ex._1 + " -> " + ex._2 + "\n"
    res
  }

  def apply(v:String): Int =
    getVal(v) match {
      case Some(x) => x
      case None => 0
    }
}

object ChoSolution {
  class MyEmptySol extends Solution {
    def hasFlowOn(end: String) = false
    def getDataOn(end: String) = None
    override def toString = "ø"
  }

  implicit object NoSol extends EmptySol[ChoSolution] {
    def sol = new ChoSolution(new CPSolver(),Map())
  }
}

