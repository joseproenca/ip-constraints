package reopp.common.guardedcommands.chocobuilder

import reopp.common.choco.{ChoConstraints, ChoSolution, ConstrBuilder}
import reopp.common.guardedcommands._
import reopp.common.{OptionSol, IntFunction, IntPredicate}
import reopp.common.guardedcommands.GuardedCom
import reopp.common.guardedcommands.IntPred
import reopp.common.guardedcommands.Var
import reopp.common
/**
 * Static library to convert a guarded command into a choco.ConstrBuilder, from which the choco constraints can be used.
 * It allows only integer constraints, solved without predicate abstraction.
 *
 *
 * Created by jose on 04/04/13.
 */
object ChocoBuilderInt {


  def solveChoco(gcs: Formula): OptionSol[ChoSolution] =
    ChoConstraints(toConstBuilders(gcs)).solve


  def toConstBuilders(gcs: Formula) :Iterable[ConstrBuilder] = {
    // not closing constraints
    for (com <- gcs.commands)
      yield toConstrBuilder(com)
  }

  private def toConstrBuilder(gc: GuardedCom): ConstrBuilder = {
    toConstrBuilder(gc.g) --> toConstrBuilder(gc.st)
  }

  private def toConstrBuilder(g: Guard): ConstrBuilder = g match {
    case Var(name) => common.choco.Var(name)
    case IntPred(v, p) => common.choco.FlowPred(p.choPred,v) // THIS is the difference with Bool below.
    case Pred(v, p) => p match {
      case intp: IntPredicate => common.choco.FlowPred(intp.choPred,v)
      case _ => throw new Exception("General predicates have no associated choco constraint")
    }
    case And(g1, g2) => toConstrBuilder(g1) and toConstrBuilder(g2)
    case Or(g1, g2) => toConstrBuilder(g1) or toConstrBuilder(g2)
    case Neg(g) => common.choco.Neg(toConstrBuilder(g))
    case Impl(g1, g2) => toConstrBuilder(g1) --> toConstrBuilder(g2)  // changed from toBoolConstrBuilder
    case Equiv(g1, g2) => toConstrBuilder(g1) <-> toConstrBuilder(g2) // changed from toBoolConstrBuilder
    case True => common.choco.TrueC
  }

  private def toConstrBuilder(st: Statement): ConstrBuilder = st match {
    case g: Guard => toConstrBuilder(g)
    case IntAssgn(v, d) => common.choco.DataAssgn(v,d)
    case DataAssgn(v, d:Int) => common.choco.DataAssgn(v,d)
    case DataAssgn(v, d: Any) =>
          throw new RuntimeException("General data assignments have no associated choco constraint")
//    case DataAssgn(v, d) =>
//      if (d.isInstanceOf[Int]) reopp.common.choco.DataAssgn(v,d.asInstanceOf[Int])
//      else throw new RuntimeException("General data assignments have no associated choco constraint")
    case VarAssgn(v1, v2) => common.choco.VarEq(v1,v2)
    case FunAssgn(v1, v2, f: IntFunction) => common.choco.FunAssgn(v1,v2,f.choFun)
    case NFunAssgn(v1,vs, f: IntFunction) =>
      throw new Exception("Choco solver does not yet support n-ary functions - "+f)
    case FunAssgn(v1, v2, f: Any) =>
          throw new RuntimeException("General data functions have no associated choco constraint")
//    case FunAssgn(v1, v2, f) =>
//      if (f.isInstanceOf[IntFunction]) reopp.common.choco.FunAssgn(v1,v2,f.asInstanceOf[IntFunction].choFun)
//      else throw new RuntimeException("General data functions have no associated choco constraint")
    case Seq(Nil) => common.choco.TrueC
    case Seq(s::ss) => toConstrBuilder(s) and toConstrBuilder(Seq(ss))
  }



}
