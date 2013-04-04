package common.guardedcommands.chocox

import common.guardedcommands._
import common.choco.genericconstraints.{PredManager}
import choco.kernel.model.variables.integer.IntegerVariable
import choco.kernel.model.constraints.{Constraint => ChocoConstr}
import common.choco.ChoSolution
import choco.cp.solver.CPSolver
import common.{Buffer, Predicate, Utils}
import choco.Choco
import collection.mutable
import choco.kernel.common.logging.{Verbosity, ChocoLogging}
import choco.cp.model.CPModel
import collection.mutable.{Set => MutSet, Map => MutMap}

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 05/02/13.
 *
 * Static library of functions to convert from guarded commands to Choco constraints,
 * using
 */
object ChocoX {
  type VarMap     = MutMap[String, IntegerVariable]
  type DataHash   = MutMap[Integer, Any]
  type FunHash    = MutMap[Integer, (common.Function, IntegerVariable)]
  type NewPredMap = MutMap[(Predicate,String), IntegerVariable]

  private def optimChocoVars(gcs: Formula,vars: VarMap) {
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
      val v = Choco.makeIntVar(name) //,-4294967296,4294967296)
      m += (name -> v)
      v
    }
  }

  /**
   * Creates a new boolean variable for Choco for a predicate, if it does not exist yet.
   * @param np existing predicate vars.
   * @param p predicate for which a variable is created.
   * @param v associsted port name.
   * @return boolean variable for 'p(v)'
   */
  private def getPred(np: NewPredMap,p: Predicate,v: String,vm: VarMap,b: Buffer,d: DataHash,f: FunHash,
                      //cs: MutSet[ChocoConstr])
                      xps: MutSet[(Predicate,String,IntegerVariable)]): IntegerVariable = {
    if (np contains (p,v)) np(p,v)
    else {
      val iv = Choco.makeBooleanVar(Utils.predVar(v,p,List()))
      np((p,v)) = iv
//      val d2: MutMap[Integer,AnyRef] = d.asInstanceOf[MutMap[Integer,AnyRef]]
////      .map((n:Integer,smt:Any) => (n,smt.asInstanceOf[AnyRef]))
//      cs += XPredManager.genConstr(getVar(vm,Utils.data2flow(v)),getVar(vm,v),iv,d2,f,b,p)
      // POSTPONE!
      xps += ((p,v,iv))
      iv
    }
  }


  /**
   * To be used by the "solve" function. Generates choco formulas with external predicates.
   * It does not rely on any of the previous ChoConstraints for better isolation.
   * @param gcs
   * @param buf
   * @return
   */
  def gc2chocox(gcs: Formula,buf: Buffer)
      :(VarMap,Iterable[ChocoConstr],DataHash,FunHash,Buffer,NewPredMap) = {
    val chocos   = MutSet[ChocoConstr]()
    val vm       = MutMap[String, IntegerVariable]()
    val datahash = MutMap[Integer, Any]()
    val funhash  = MutMap[Integer, (common.Function, IntegerVariable)]()
    val newpred  = MutMap[(Predicate,String), IntegerVariable]()
    val xpredicates = MutSet[(Predicate,String,IntegerVariable)]()

    for (gc <-  gcs.commands) {
      gc2chocox(gc,vm,buf,datahash,funhash,newpred,chocos,xpredicates)
    }
    for ((p,v,iv) <- xpredicates) {
      val d2: MutMap[Integer,AnyRef] = datahash.asInstanceOf[MutMap[Integer,AnyRef]]
      chocos += XPredManager.genConstr(getVar(vm,Utils.data2flow(v)),getVar(vm,v),iv,d2,funhash,buf,p)
    }
    optimChocoVars(gcs,vm) // replace "a = b" by "a = a" in the choco constraints
//    gcs.close() // add some-flow condition
    (vm, chocos,datahash,funhash,buf, newpred)
  }

  private def gc2chocox(gc:GuardedCom,vm:VarMap,b: Buffer,d: DataHash,f: FunHash,p: NewPredMap,
                        cs: MutSet[ChocoConstr],xps: MutSet[(Predicate,String,IntegerVariable)]) {
    if (gc.g == True)
      cs += gc2chocox(gc.st,gc.g,vm,b,d,f,p,xps)
    else
      cs += Choco.or(Choco.not(gc2chocox(gc.g,vm,b,d,f,p,xps)),
                     gc2chocox(gc.st,gc.g,vm,b,d,f,p,xps))
  }

  private def gc2chocox(g:Guard,vm:VarMap,b: Buffer,d: DataHash,f: FunHash,np: NewPredMap,
                        xps: MutSet[(Predicate,String,IntegerVariable)]):
                        ChocoConstr = g match {
    case Pred(v, p) =>
      val newvar = getPred(np,p,v,vm,b,d,f,xps)
      Choco.eq(newvar,1)
    //////
    case IntPred(v, p) => gc2chocox(Pred(v,p),vm,b,d,f,np,xps)
    case Var(name) => Choco.eq(getVar(vm,name), 1)
    case And(g1, g2) => Choco.and(gc2chocox(g1,vm,b,d,f,np,xps),gc2chocox(g2,vm,b,d,f,np,xps))
    case Or(g1, g2) => Choco.or(gc2chocox(g1,vm,b,d,f,np,xps),gc2chocox(g2,vm,b,d,f,np,xps))
    case Neg(g1) =>    Choco.not(gc2chocox(g1,vm,b,d,f,np,xps))
    case Impl(g1, g2) => gc2chocox((!g1) \/ g2,vm,b,d,f,np,xps)
    case Equiv(g1, g2) => gc2chocox((g1 -> g2) /\ (g2 -> g1),vm,b,d,f,np,xps)
    case True => Choco.TRUE
  }


  /**
   * Generate a choco expression for `st`, under the guard `g`.
   * @param st Statement to be converted
   * @param g Guard associated to the statement
   * @param vm map from var names to choco vars
   * @param b buffer
   * @param d map from the hash of some (var,guard) to a data value
   * @param f map from the hash of some (var,guard) to a pair (function, choco var)
   * @param np new variables, one for each external predicate applied to a different var
//   * @param cs global buffer of choco constraints, to add external predicates
   * @param xps global buffer of external predicates that must be built after collecting all other constraints
   * @return new choco expression, possibly modifying mutable arguments.
   */
  private def gc2chocox(st:Statement,g:Guard,vm:VarMap,b: Buffer,d: DataHash,f: FunHash,np: NewPredMap,
                        xps: MutSet[(Predicate,String,IntegerVariable)]):
                        ChocoConstr = st match {
    case DataAssgn(v, dt) =>
      val hash = (v, g).hashCode()
      val hash2 = (math.abs(hash) % 21474836) * math.signum(hash)
      d(hash2) = dt
      Choco.eq(getVar(vm,v),hash2)
    case FunAssgn(v1, v2, fn) => // v1 = f(v2)
      val hash = (v1,g).hashCode()
      val hash2 = (math.abs(hash) % 21474836) * math.signum(hash)
      f(hash2) = (fn,getVar(vm,v2))
      Choco.eq(getVar(vm,v1),hash2)
    //////
    case IntAssgn(v, dt) => gc2chocox(DataAssgn(v,Int.box(dt)),g,vm,b,d,f,np,xps)
    case g2: Guard => gc2chocox(g2,vm,b,d,f,np,xps)
    case VarAssgn(v1, v2) => Choco.eq(getVar(vm,v1),getVar(vm,v2))
    case Seq(Nil) => Choco.TRUE
    case Seq(s::Nil) => gc2chocox(s,g,vm,b,d,f,np,xps)
    case Seq(s::ss) => Choco.and(gc2chocox(s,g,vm,b,d,f,np,xps),gc2chocox(Seq(ss),g,vm,b,d,f,np,xps))
  }


  def solve(gcs: Formula,buf: Buffer): Option[CXSolution] = {
    val DEBUG = false
    ChocoLogging.setVerbosity(Verbosity.OFF)

//    val buf = new Buffer
//    val datahash = mutable.Map[Int,AnyRef]()
//    val funhash  = mutable.Map[Int,(common.Function,IntegerVariable)]()
    val s = new CPSolver

    val m: CPModel = new CPModel

    val tuple = gc2chocox(gcs,buf) //ConstrBuilder.toChoco(constrBuilders, buf)
    val varMap = tuple._1
    val datahash = tuple._3
    val funhash = tuple._4
    val buffer = tuple._5
    val newpred = tuple._6
    for (constr <- tuple._2)
      m.addConstraint(constr)

    if (DEBUG) println("%%%%%%% got model: "+m.pretty())
    val it = m.getConstraintIterator
    if (DEBUG) {
      println("pp:")
      while (it.hasNext)
        println(PredManager.prettyConst(it.next()))
      println("%%%%%%%")
      println("datahash:\n"+datahash.mkString("\n"))
      println("funhash:\n"+funhash.mkString("\n"))
      println("%%%%%%%")

      println("### reading")
    }

    s.read(m)
    if (DEBUG) println("### read")

    val solved = s.solve()

    if (DEBUG){
      println("### solved")
      println("datahash:\n"+datahash.mkString("\n"))
      println("funhash:\n"+funhash.mkString("\n"))
    }

    //    println(s.pretty())

    val res =
      if (solved) Some(new CXSolution(s,varMap.toMap,buffer,datahash.toMap,funhash.toMap,newpred.toMap))
      else  None

    if (DEBUG) {
      if (solved) println(res.get)
      else println("no sol")
    }

    res
  }
}
