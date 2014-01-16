package reopp.common.guardedcommands.chocobuilder

import reopp.common.guardedcommands._
import reopp.common.choco.{ChoConstraints, LazyPred, TrueC, ConstrBuilder}
import reopp.common.Utils._
import reopp.common.guardedcommands.GuardedCom
import reopp.common.{NoneSol, SomeSol, OptionSol, Buffer}
import reopp.common
/**
 * Static library to convert a guarded command into a choco.ConstrBuilder, from which the choco constraints can be used.
 *
 * Created by jose on 04/04/13.
 */
object ChocoBuilderSAT {

  /** Solve a set of constraint builders from gc2BoolConstrBuilders.
   *
   * @param buf cache computed data here
   * @param da domain invariants and information about the order of variables
   * @param builders constraints to be solved using ChoConstraints
   * @return possible solution
   */
  def solveChocoBool(buf: Buffer, da: DomainAbst, builders: Iterable[ConstrBuilder]) : OptionSol[GCBoolSolution] = {
    val choSol = ChoConstraints(builders).solve(da.guessOrder,buf)
//    for (s <- choSol) yield new GCBoolSolution(s.sol2map)
    if (choSol.isDefined) SomeSol(new GCBoolSolution(choSol.get.sol2map,Some(buf)))
    else NoneSol(buf)
  }

  /** Solve a set of constraint builders from gc2BoolConstrBuilders.
    *
    * @param da domain invariants and information about the order of variables
    * @param builders constraints to be solved using ChoConstraints
    * @return possible solution
    */
  def solveChocoBool(da: DomainAbst, builders: Iterable[ConstrBuilder]) : OptionSol[GCBoolSolution] =
    solveChocoBool(new Buffer, da, builders)


  /**
   * Calculate the choco integer constraints for the abstract (SAT) problem.
   * @return A constraint builder for choco constraints.
   */
  def gc2BoolConstrBuilders(gc: Formula, da: DomainAbst): Iterable[ConstrBuilder] = {

    for (com <- gc.commands)
      yield gc2BoolConstrBuilder(com,da)
  }


  /**
   * Predicate abstraction + convertion to ConstrBuilder (core of Choco constraints).
   * All predicates are treated as lazy impure functions.
   * @param com guarded command to be converted
   * @param da Domain abstraction (domain invariants)
   * @return
   */
  def gc2BoolConstrBuilder(com: GuardedCom, da: DomainAbst): ConstrBuilder = {
    gc2BoolConstrBuilder(com.g) --> gc2BoolConstrBuilder(com.st, da)

  }


  def gc2BoolConstrBuilder(g:Guard): ConstrBuilder = g match {
    case Var(name) => common.choco.Var(name)
    case IntPred(v, p) => common.choco.Var(predVar(v,p,List()))//reopp.common.choco.FlowPred(p.choPred,v)
    case Pred(v, p) => common.choco.Var(predVar(v,p,List()))
    case And(g1, g2) => gc2BoolConstrBuilder(g1) and gc2BoolConstrBuilder(g2)
    case Or(g1, g2) =>  gc2BoolConstrBuilder(g1) or  gc2BoolConstrBuilder(g2)
    case Neg(g) => common.choco.Neg(gc2BoolConstrBuilder(g))
    case Impl(g1, g2) => gc2BoolConstrBuilder(g1) --> gc2BoolConstrBuilder(g2)
    case Equiv(g1, g2) => gc2BoolConstrBuilder(g1) <-> gc2BoolConstrBuilder(g2)
    case True => common.choco.TrueC
  }

  /**
   * Performs predicate abstraction and returns a boolean formula.
   * It produces a Choco constraint (a ConstrBuilder), and uses lazy constraints.
   * @param st The statement to be converted
   * @param da domain abstraction (what needs to be precomputed)
   * @return Boolean Choco constraint (constraint builder)
   */
  def gc2BoolConstrBuilder(st:Statement, da: DomainAbst): ConstrBuilder = st match {
    case IntAssgn(v, d) => gc2BoolConstrBuilder(DataAssgn(v,Int.box(d)), da)
    case DataAssgn(v, d) => //reopp.common.choco.DataAssgn(v,d)
      // INSTEAD OF CALCULATING, CREATE A LAZY CONSTRAINT!
      // TODO: CREATE new temp var 'predvar2(v,pred,fs)' - it will be the output var of the lazy pred (to confirm...)
      var res:ConstrBuilder = TrueC
      for ((pred,fs,xflow) <- da.domainWithEnd(v)) {

        //        println("added LazyPred("+predVar(v,pred,fs)+","+data2flow(v)+","+data2flow(xflow)+","+fs+")")
        res = res and LazyPred(predVar(v,pred,fs),data2flow(v),data2flow(xflow),d,pred,fs)
      }

      //        var newd = d
      //        for (f<-fs.reverse) newd = f.calculate(newd)
      //        if (pred.check(newd))
      //          res = res and reopp.common.choco.Var(predVar(v,pred,fs))
      //        else
      //          res = res and reopp.common.choco.Neg(reopp.common.choco.Var(predVar(v,pred,fs)))

      res

    case VarAssgn(v1, v2) =>
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: ConstrBuilder= TrueC
      for ((pred,fs) <- d2)
        if (d1 contains (pred,fs)) {
          val t = common.choco.VarEq(predVar(v1,pred,fs),predVar(v2,pred,fs))
          res = res and t
        }
      res

    case FunAssgn(v1,v2,f) =>
      //      VarAssgn(v1,v2).toBoolConstrBuilder(da)
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      //      println("domains in fun assign:\n"+d1+"\n"+d2)
      var res: ConstrBuilder= TrueC
      for ((pred,fs) <- d1)
        if (d2 contains (pred,fs++List(f))) {
          val t = common.choco.VarEq(predVar(v1,pred,fs),predVar(v2,pred,fs++List(f)))
          //          println("adding "+predVar(v1,pred,fs)+" == "+predVar(v2,pred,fs++List(f)))
          res = res and t
        }
      //        else
      //          println("NOT adding "+predVar(v1,pred,fs)+" == "+predVar(v2,pred,fs++List(f)) ++
      //          "  --  "+d2+" does  not contain "+(pred,fs++List(f)))
      res
    case NFunAssgn(v,v2s,f) => v2s match {
      case List(v2) => gc2BoolConstrBuilder(FunAssgn(v,v2,f),da)
      case Nil => throw new Exception("Predicate abstraction cannot be applied to n-ary functions - "+f)
    }

    case Seq(Nil) => common.choco.TrueC
    case Seq(s::ss) => gc2BoolConstrBuilder(s,da) and gc2BoolConstrBuilder(Seq(ss),da)
    //    case g: Guard => super.toBoolConstrBuilder
    case g: Guard => gc2BoolConstrBuilder(g)
  }

}
