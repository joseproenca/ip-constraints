package common.guardedcommands.chocox

import common.guardedcommands._
import common.choco.genericconstraints.Buffer
import choco.kernel.model.variables.integer.IntegerVariable
import choco.kernel.model.constraints.{Constraint => ChocoConstr}
import common.choco.ChoSolution
import choco.cp.solver.CPSolver
import common.Utils
import choco.Choco
import collection.mutable
import choco.kernel.common.logging.{Verbosity, ChocoLogging}
import choco.cp.model.CPModel

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 05/02/13.
 */
object ChocoX {
  type VarMap = mutable.Map[String, IntegerVariable]
  type DataHash = mutable.Map[Int, AnyRef]
  type FunHash = mutable.Map[Int, (common.Function, IntegerVariable)]

  private def optimChocoVars(gcs: GuardedCommands,vars: VarMap) {
    for (gc <- gcs.commands)
      if (gc.g == True)
        optimChocoVars(gc.st,vars)
    vars
  }

  private def optimChocoVars(s: Statement,vars: VarMap): Unit = s match {
    case VarAssgn(v1, v2) =>
      if (vars contains v1)
        vars += (v2 -> vars(v1))
      else {
        val v2var = getVar(vars,v2)
        vars += (v1 -> v2var)
      }
    case _: Guard =>
    case IntAssgn(v, d) =>
    case FunAssgn(v1, v2, f) =>
    case DataAssgn(v, d) =>
    case Seq(Nil) =>
    case Seq(st::sts) => {optimChocoVars(st,vars);optimChocoVars(Seq(sts),vars)}
  }


  /**
   * Retrieves an IntegerVariable from a map if it exists, or creates one if it does not exist, updating the map.
   * @param m map from variable names (Strings) to Choco variables (IntegerVariables)
   * @param name of the variable (String)
   * @return Choco variable (IntegerVariable) for the given variable name
   */
  private def getVar(m: VarMap, name: String): IntegerVariable = {
    if (m contains name)
      m(name)
    else if (Utils.isFlowVar(name)) {
      val v = Choco.makeBooleanVar(name)
      m += (name -> v)
      v
    }
    else if (Utils.isPredVar(name)) {
      val v = Choco.makeBooleanVar(name)
      m += (name -> v)
      v
    }
    else {
      val v = Choco.makeIntVar(name)
      m += (name -> v)
      v
    }
  }


  /**
   * To be used by the "solve" function. Generates choco formulas with external predicates.
   * It does not rely on any of the previous ChoConstraints for better isolation.
   * @param gcs
   * @param buf
   * @param datahash
   * @param funhash
   * @return
   */
  def gc2chocox(gcs: GuardedCommands,buf: Buffer,datahash: DataHash,funhash: FunHash): (VarMap,Iterable[ChocoConstr]) = {
    val chocos = mutable.Set[ChocoConstr]()
    val vm = mutable.Map[String, IntegerVariable]()
    for (gc <-  gcs.commands) {
      gc2chocox(gc,vm,buf,datahash,funhash,chocos)
    }
    optimChocoVars(gcs,vm)
    (vm, chocos)
  }

  private def gc2chocox(gc:GuardedCom,vm:VarMap,buf: Buffer,datahash: DataHash,funhash: FunHash,chocos: mutable.Set[ChocoConstr]) {
    if (gc.g != True)
      chocos += gc2chocox(gc.g,vm,buf,datahash,funhash)
    chocos += gc2chocox(gc.st,vm,buf,datahash,funhash,chocos)
  }

  private def gc2chocox(g:Guard,vm:VarMap,b: Buffer,d: DataHash,f: FunHash): ChocoConstr = {
    g match {
      case IntPred(v, p) => null //...
      case Pred(v, p) =>    null //...
      //////
      case Var(name) => Choco.eq(getVar(vm,name), 1)
      case And(g1, g2) => Choco.and(gc2chocox(g1,vm,b,d,f),gc2chocox(g2,vm,b,d,f))
      case Or(g1, g2) => Choco.or(gc2chocox(g1,vm,b,d,f),gc2chocox(g2,vm,b,d,f))
      case Neg(g1) =>    Choco.not(gc2chocox(g1,vm,b,d,f))
      case Impl(g1, g2) => gc2chocox((!g1) \/ g2,vm,b,d,f)
      case Equiv(g1, g2) => gc2chocox((g1 -> g2) /\ (g2 -> g1),vm,b,d,f)
      case True => Choco.TRUE
    }
  }

  private def gc2chocox(st:Statement,vm:VarMap,b: Buffer,d: DataHash,f: FunHash,cs: mutable.Set[ChocoConstr]): ChocoConstr = {
    st match {
      case IntAssgn(v, d) =>      null // ...
      case FunAssgn(v1, v2, f) => null // ...
      case DataAssgn(v, d) =>     null // ..
      //////
      case g: Guard => gc2chocox(g,vm,b,d,f,cs)
      case VarAssgn(v1, v2) => Choco.eq(getVar(vm,v1),getVar(vm,v2))
      case Seq(Nil) => Choco.TRUE
      case Seq(s::Nil) => gc2chocox(s,vm,b,d,f,cs)
      case Seq(s::ss) => Choco.and(gc2chocox(s,vm,b,d,f,cs),gc2chocox(Seq(ss),vm,b,d,f,cs))
    }
  }

  // TODO
  def solve(gcs: GuardedCommands): Option[ChoSolution] = {
    ChocoLogging.setVerbosity(Verbosity.OFF)

    val buf = new Buffer
    val datahash = mutable.Map[Int,AnyRef]()
    val funhash  = mutable.Map[Int,(common.Function,IntegerVariable)]()
    val s = new CPSolver

    val m: CPModel = new CPModel

    val pair = gc2chocox(gcs,buf,datahash,funhash) //ConstrBuilder.toChoco(constrBuilders, buf)
    val varMap = pair._1
    for (constr <- pair._2)
      m.addConstraint(constr)

    //    // Add flow constraints
    //    var flowvars = Set[IntegerVariable]()
    //    for (x <- m.getIntVarIterator) {
    //      if (isFlowVar(x.getName)) flowvars += x
    //    }
    //    if (!(flowvars.isEmpty)) {
    //      var c = Choco.eq(flowvars.head,1)
    //      for (v <- flowvars.tail)
    //        c = Choco.or(Choco.eq(v,1),c)
    //      m.addConstraint(c)
    //    }

    //    println(m.pretty())

    s.read(m)

    val solved = s.solve()

    if (solved) Some(new ChoSolution(s,varMap.toMap))
    else  None
  }
}
