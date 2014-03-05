package reopp.common.guardedcommands.chocodyn

import collection.mutable.{Map => MutMap, Set => MutSet}
import _root_.choco.kernel.model.variables.integer.IntegerVariable
import reopp.common._
import reopp.common.guardedcommands._
import _root_.choco.kernel.model.constraints.{Constraint => ChocoConstr}
import _root_.choco.Choco
import reopp.common.choco.ChoUtils._
import _root_.choco.kernel.common.logging.{Verbosity, ChocoLogging}
import _root_.choco.cp.solver.CPSolver
import _root_.choco.cp.model.CPModel
import reopp.common.choco.genericconstraints.PredManager
import reopp.common.guardedcommands.Or
import reopp.common.guardedcommands.Equiv
import reopp.common.guardedcommands.Neg
import reopp.common.guardedcommands.IntPred
import reopp.common.guardedcommands.GuardedCom
import reopp.common.guardedcommands.FunAssgn
import reopp.common.guardedcommands.VarAssgn
import reopp.common.guardedcommands.Var
import reopp.common.guardedcommands.DataAssgn
import scala.Some
import reopp.common.guardedcommands.Pred
import reopp.common.guardedcommands.Impl
import reopp.common.guardedcommands.And
import reopp.common.guardedcommands.Seq
import reopp.common.guardedcommands.NFunAssgn
import reopp.common.guardedcommands.IntAssgn


/**
 * Static libraries to convert a formula to a choco constraints with a dynamic table of data being sent.
 *
 * Created by jose on 05/04/13.
 */
object ChocoDyn {

  type VarMap  = MutMap[String, IntegerVariable] // mapping: varName  -> varChoco
  type NewPred = MutMap[String, IntegerVariable] // mapping: predName -> varChoco



  def gc2ChocoDyn(gcs: Formula, buf: Buffer):
      (Iterable[ChocoConstr],VarMap, DataMap, Buffer, NewPred)  = {

    val chocos   = MutSet[ChocoConstr]()
    val dm: DataMap = new DataMap()                     // mapping: index -> real data (used dynamically)
    val vm: VarMap  = MutMap[String, IntegerVariable]() // mapping: varName  -> varChoco
    val np: NewPred = MutMap[String, IntegerVariable]() // mapping: predName -> varChoco


    for (gc <-  gcs.commands) {
      gc2ChocoDyn(chocos,gc,vm,dm,buf,np)
    }

    optimChocoVars(gcs,vm) // replace "a = b" by "a = a" in the choco constraints
    //    gcs.close() // add some-flow condition
    (chocos, vm, dm, buf, np)
  }


  private def gc2ChocoDyn(cs: MutSet[ChocoConstr],gc: GuardedCom, vm: VarMap, dm: DataMap, b:Buffer, np: NewPred) {
    if (gc.g == True)
      cs += gc2ChocoDyn(cs,gc.st,vm,dm,b,np)
    else
      cs += Choco.or(Choco.not(gc2ChocoDyn(cs,gc.g,vm,dm,b,np)),
        gc2ChocoDyn(cs,gc.st,vm,dm,b,np))
  }

  private def gc2ChocoDyn(cs: MutSet[ChocoConstr],g: Guard, vm: VarMap, dm: DataMap, b:Buffer, np: NewPred):
       ChocoConstr = g match {
    case Pred(v, p) =>
      val newvar = addPred(v,p, vm, dm, b, np, cs)
      Choco.eq(newvar,1)
    //////
    case IntPred(v, p) => gc2ChocoDyn(cs,Pred(v,p),vm,dm,b,np)
    case Var(name) => Choco.eq(getVar(vm,name), 1)
    case And(g1, g2) => Choco.and(gc2ChocoDyn(cs,g1,vm,dm,b,np),gc2ChocoDyn(cs,g2,vm,dm,b,np))
    case Or(g1, g2) => Choco.or(gc2ChocoDyn(cs,g1,vm,dm,b,np),gc2ChocoDyn(cs,g2,vm,dm,b,np))
    case Neg(g1) =>    Choco.not(gc2ChocoDyn(cs,g1,vm,dm,b,np))
    case Impl(g1, g2) => gc2ChocoDyn(cs,(!g1) \/ g2,vm,dm,b,np)
    case Equiv(g1, g2) => gc2ChocoDyn(cs,(g1 -> g2) /\ (g2 -> g1),vm,dm,b,np)
    case True => Choco.TRUE
  }


  private def addPred(v: String, p: Predicate, vm: VarMap, dm: DataMap, b:Buffer, np: NewPred, cs: MutSet[ChocoConstr]):
      IntegerVariable = {
    val name = Utils.predVar(v,p,List())
    if (np contains name) np(name)
    else {
      val iv = Choco.makeBooleanVar(name)
      np(name) = iv
      cs += DynPredManager.genPredicate(
        // BoolVar of v              ,DataVar of v, PredVar,dm,b,predicate
        getVar(vm,Utils.flowVar(v)),getVar(vm,v),iv      ,dm,b,p)
      iv
    }
  }

  private def gc2ChocoDyn(cs: MutSet[ChocoConstr],st: Statement, vm: VarMap, dm: DataMap, b:Buffer, np: NewPred):
      ChocoConstr = st match {
    case DataAssgn(v, dt) =>
      Choco.eq(getVar(vm,v),dm.add(dt))
    case FunAssgn(v1, v2:String, fn) => // v1 = f(v2)
      cs += DynFuncManager.genFunction(
        // boolvar of v1             , datavar of v1, boolvar of v2               , datavar of v2,dm,b,function
        getVar(vm,Utils.flowVar(v1)),getVar(vm,v1) ,getVar(vm,Utils.flowVar(v2)),getVar(vm,v2) ,dm,b,fn)
      Choco.TRUE
    case NFunAssgn(v1, v2s:List[String], fn) => // v1 = f(v2s)
      cs += DynNFuncManager.genNFunction(
        // boolvar of v1             , datavar of v1,
        getVar(vm,Utils.flowVar(v1)),getVar(vm,v1) ,
        // boolvars of v2                          ,
        v2s.map(v2=>getVar(vm,Utils.flowVar(v2))),
        // datavar of v2           ,dm,b,function
        v2s.map(v2=>getVar(vm,v2)) ,dm,b,fn)
      Choco.TRUE
    //////
    case IntAssgn(v, dt) => gc2ChocoDyn(cs,DataAssgn(v,Int.box(dt)),vm,dm,b,np)
    case g2: Guard => gc2ChocoDyn(cs,g2,vm,dm,b,np)
    case VarAssgn(v1, v2) => Choco.eq(getVar(vm,v1),getVar(vm,v2))
    case Seq(Nil) => Choco.TRUE
    case Seq(s::Nil) => gc2ChocoDyn(cs,s,vm,dm,b,np)
    case Seq(s::ss) => Choco.and(gc2ChocoDyn(cs,s,vm,dm,b,np),gc2ChocoDyn(cs,Seq(ss),vm,dm,b,np))
  }



  def solve(gcs: Formula,tried:Option[NoneSol]): OptionSol[DynSolution] = {
    val buf = tried match {
      case Some(NoneSol(Some(buf:Buffer))) => buf
      case _ => new Buffer
    } 

    val DEBUG = false
    ChocoLogging.setVerbosity(Verbosity.OFF)

    //    val buf = new Buffer
    //    val datahash = mutable.Map[Int,AnyRef]()
    //    val funhash  = mutable.Map[Int,(reopp.common.Function,IntegerVariable)]()
    val s = new CPSolver

    val m: CPModel = new CPModel

    // (Iterable[ChocoConstr],VarMap, DataMap, Buffer, NewPred)  = {
    val tuple = gc2ChocoDyn(gcs,buf) //ConstrBuilder.toChoco(constrBuilders, buf)
    val varMap = tuple._2
    val dataMap = tuple._3
    val buffer = tuple._4
    val np  = tuple._5
    for (constr <- tuple._1)
      m.addConstraint(constr)

    if (DEBUG) println("%%%%%%% got model: "+m.pretty())
    val it = m.getConstraintIterator
    if (DEBUG) {
      println("pp:")
      while (it.hasNext)
        println(PredManager.prettyConst(it.next()))
      println("%%%%%%%")
      println("dataMap:\n"+dataMap.getAll.mkString("\n"))
      println("%%%%%%%")

      println("### reading")
    }

    s.read(m)
    if (DEBUG) println("### read")

    val solved = s.solve()

    if (DEBUG){
      println("### solved")
      println("datamap:\n"+dataMap.getAll.mkString("\n"))
    }

    //    println(s.pretty())

    val res =
      if (solved) SomeSol(new DynSolution(s,varMap.toMap,buffer,dataMap,np))
      else  NoneSol(buf)

    if (DEBUG) {
      if (solved) println(res.get)
      else println("no sol")
    }

    res
  }

}
