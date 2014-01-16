package reopp.common.choco

import reopp.common.{guardedcommands => f}
import choco.kernel.model.variables.integer.IntegerVariable
import reopp.common.Utils.{isFlowVar,isPredVar}
import choco.Choco
import collection.mutable.{Map => MutMap}

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 05/04/13.
 */
object ChoUtils {
  private type VarMap     = MutMap[String, IntegerVariable]


  def optimChocoVars(gcs: f.Formula,vars: VarMap) {
    for (gc <- gcs.commands)
      if (gc.g == f.True)
        optimChocoVars(gc.st,vars)
    vars
  }

  private def optimChocoVars(s: f.Statement,vars: VarMap): Unit = s match {
    case f.VarAssgn(v1, v2) =>
      if (vars contains v1)
        vars += (v2 -> vars(v1))
      else {
        val v2var = getVar(vars,v2)
        vars += (v1 -> v2var)
      }
    case _: f.Guard =>
    case f.IntAssgn(v, d) =>
    case f.FunAssgn(v1, v2, f) =>
    case f.NFunAssgn(v1, vs, f) =>
    case f.DataAssgn(v, d) =>
    case f.Seq(Nil) =>
    case f.Seq(st::sts) => {optimChocoVars(st,vars);optimChocoVars(f.Seq(sts),vars)}
  }


  /**
   * Retrieves an IntegerVariable from a map if it exists, or creates one if it does not exist, updating the map.
   * @param m map from variable names (Strings) to Choco variables (IntegerVariables)
   * @param name of the variable (String)
   * @return Choco variable (IntegerVariable) for the given variable name
   */
  def getVar(m: VarMap, name: String, nats:Boolean = false): IntegerVariable = {
    if (m contains name)
      m(name)
    else if (isFlowVar(name)) {
      val v = Choco.makeBooleanVar(name)
      m += (name -> v)
      v
    }
    else if (isPredVar(name)) {
      val v = Choco.makeBooleanVar(name)
      m += (name -> v)
      v
    }
    else if (nats) {
      val v = Choco.makeIntVar(name,0,Integer.MAX_VALUE)
      m += (name -> v)
      v
    }
    else {
      val v = Choco.makeIntVar(name) //,-4294967296,4294967296)
      m += (name -> v)
      v
    }
  }

}
