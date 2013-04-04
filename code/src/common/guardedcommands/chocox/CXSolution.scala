package common.guardedcommands.chocox

import common.choco.ChoSolution
import choco.cp.solver.CPSolver
import choco.kernel.model.variables.integer.IntegerVariable
import common.{Buffer, Predicate, EmptySol, Solution}
import common.guardedcommands.GCSolution

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 06/02/13.
 */
class CXSolution(choSol: CPSolver, varMap:Map[String,IntegerVariable],
                 b: Buffer, datahash:Map[Integer, Any],
                 funhash:Map[Integer,(common.Function,IntegerVariable)],
                 newpred:Map[(Predicate,String), IntegerVariable])
  extends GCSolution(GCSolution.NoSol.sol,varMap) {

//  println("datahash:\n"+datahash.mkString("/n"))
//  println("funhash:\n"+funhash.mkString("/n"))
//  println("solution:\n"+this)

  override def getDataOn(v: String): Option[Any] = {
    if (varMap contains v)
      getDataOn(varMap(v))
    else
      None
  }

  def getDataOn(v: IntegerVariable): Option[Any] =
    if (choSol contains v) {
      val res = choSol.getVar(v).getVal
      if (datahash contains res)
        Some(datahash(res))
      else if (funhash contains res) {
        val arg = getDataOn(funhash(res)._2)
        if (arg.isDefined)
          Some(b.calculate(List(funhash(res)._1),arg.get))
        else
          Some(res)
      }
      else
        Some(res)
    }
    else None


  override def hasFlowOn(v: String) = {
    //    println("has flow on '"+v+"'? - getVal: "+ getVal(v))
    getDataOn(v) == Some(1)
  }

  def size = varMap.size
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


    for ((k,v) <- varMap.toList.sortBy((x:(String,IntegerVariable)) => x._1)) {
      val dt = getDataOn(v)
      if (dt.isDefined)
        res += common.Utils.ppVar(k) + " -> " + dt.get + "\n"
//      if (choSol.contains(v)) {
//        val value = choSol.getVar(v).getVal
//        if (datahash contains value)
//          res += common.Utils.ppVar(k) + " -> " + datahash(value) + "\n"
//        else
//          res += common.Utils.ppVar(k) + " -> " + value + "\n"
//      }
      else
        res += common.Utils.ppVar(k) + " -> NOFLOW\n"
    //      res += k +" '"+v + "'\n"
    }
    for (((_,s), iv) <- newpred) {
      val dt = getDataOn(iv)
      if (dt.isDefined)
//        res += common.Utils.ppPredVar(s) + " -> " + dt.get + "\n"
//        res += common.Utils.ppPredVar(iv.getName) + " -> " + dt.get + "\n"
        res += iv.getName + " -> " + dt.get + "\n"
      else
        res += common.Utils.ppPredVar(s) + " -> NOFLOW\n"
    }

    res
  }

  override def apply(v:String): Any =
    getDataOn(v) match {
      case Some(x) => x
      case None => 0
    }
}

object CXSolution {
  class MyEmptySol extends Solution {
    def hasFlowOn(end: String) = false
    def getDataOn(end: String) = None
    override def toString = "ø"
  }

  implicit object NoSol extends EmptySol[CXSolution] {
    def sol = new CXSolution(new CPSolver(),Map(),new Buffer,Map(),Map(),Map())
  }
}