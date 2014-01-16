package reopp.common.guardedcommands.chocodyn

import reopp.common.{EmptySol, Buffer, Solution}
import choco.cp.solver.CPSolver
import choco.kernel.model.variables.integer.IntegerVariable
import reopp.common.guardedcommands.GCSolution
import collection.mutable
import reopp.common

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 05/04/13.
 */
class DynSolution(choSol: CPSolver, varMap:Map[String,IntegerVariable],
                   b: Buffer, datamap:DataMap,
                   newpred: mutable.Map[String, IntegerVariable])
  extends GCSolution(GCSolution.NoSol.sol,varMap) {

  //  println("datahash:\n"+datahash.mkString("/n"))
  //  println("funhash:\n"+funhash.mkString("/n"))
  //  println("solution:\n"+this)
  override def getBuffer = Some(b)

  override def getDataOn(v: String): Option[Any] = {
    if (varMap contains v)
      getDataOn(varMap(v))
    else
      None
  }

  def getDataOn(v: IntegerVariable): Option[Any] =
    if (choSol contains v) {
      val idx = choSol.getVar(v).getVal
      if (idx > 1) {
        val res = datamap.get(idx)
        if (res != null)
          Some(res)
        else
          None
      }
      else
        Some(idx)
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
      else
        res += common.Utils.ppVar(k) + " -> Undefined\n"
    }

    res
  }

  override def apply(v:String): Any =
    getDataOn(v) match {
      case Some(x) => x
      case None => 0
    }
}

object DynSolution {
  class MyEmptySol extends Solution {
    def hasFlowOn(end: String) = false
    def getDataOn(end: String) = None
    override def toString = "ø"
  }

  implicit object NoSol extends EmptySol[DynSolution] {
    def sol = new DynSolution(new CPSolver(),Map(),new Buffer, new DataMap(),mutable.Map())
  }
}