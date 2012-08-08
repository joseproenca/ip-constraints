package common.beh.guardedcommands.z3

import _root_.z3.scala._
import common.beh.guardedcommands._
import common.beh.guardedcommands.Var
import common.beh.{IntPredicate, Even, guardedcommands}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 31/07/12
 * Time: 12:30
 * To change this template use File | Settings | File Templates.
 */

object Z3 {

  def solvez3(const: Z3AST, z3: Z3Context): Option[Z3Solution] = {
    z3.assertCnstr(const)

    z3.checkAndGetModel() match {
      case (None, m) =>
        println("Z3 failed. The reason is: " + z3.getSearchFailure.message)
        None
      case (Some(false), m) =>
        println("Unsat.")
        None
      case (Some(true), model) => {
        //        for (c <- model.getModelConstants) println ("const "+c)
        //        println("v1: " + model.evalAs[Boolean](v1))
        //        println("v2: " + model.evalAs[Boolean](v2))
        //        println("v3: " + model.evalAs[Boolean](v3))
//        println("model:\n"+model)
        Some(new Z3Solution(z3,model))
        //        model.delete
      }
    }
  }

  def gc2z3(gcs: GuardedCommands, z3: Z3Context): Z3AST = {
    var res = z3.mkTrue()
    for (com <- gcs.commands)
      res = z3.mkAnd(res,gc2z3(com,z3))
    val vars = gcs.fv()

    // ADD FLOW CONSRAINTS
    if (!vars.isEmpty) {
      val fst = vars.head
      var flowConstraint = z3.mkBoolConst(z3.mkStringSymbol(fst))
      for (v <- vars.tail)
        flowConstraint = z3.mkOr(flowConstraint,z3.mkBoolConst(z3.mkStringSymbol(v)))
      res = z3.mkAnd(res,flowConstraint)
    }

    res
  }

  def gc2z3(gc: GuardedCom, z3: Z3Context): Z3AST =
    z3.mkImplies(gc2z3(gc.g,z3), gc2z3(gc.st,z3))

  def gc2z3(g: Guard,z3: Z3Context): Z3AST = g match {
    case Var(name) => z3.mkBoolConst(z3.mkStringSymbol(name))
    case IntPred(v, p) => p.z3Pred(z3,z3.mkIntConst(z3.mkStringSymbol(v)))
    case Pred(v, p) =>
      if (p.isInstanceOf[IntPredicate])
        p.asInstanceOf[IntPredicate].z3Pred(z3,z3.mkIntConst(z3.mkStringSymbol(v)))
      else
        throw new RuntimeException("General predicates not handled with Z3")
    case And(g1, g2) => z3.mkAnd(gc2z3(g1,z3),gc2z3(g2,z3))
    case Or(g1, g2) => z3.mkOr(gc2z3(g1,z3),gc2z3(g2,z3))
    case Neg(g) => z3.mkNot(gc2z3(g,z3))
    case Impl(g1, g2) => z3.mkImplies(gc2z3(g1,z3),gc2z3(g2,z3))
    case guardedcommands.Equiv(g1, g2) => z3.mkEq(gc2z3(g1,z3),gc2z3(g2,z3))
    case True => z3.mkTrue()
  }

  def gc2z3(s: Statement, z3: Z3Context): Z3AST = s match {
    case g: Guard => gc2z3(g,z3)
    case IntAssgn(v, d) => z3.mkEq(z3.mkIntConst(z3.mkStringSymbol(v)),z3.mkInt(d,z3.mkIntSort()))
    case VarAssgn(v1, v2) => z3.mkEq(z3.mkIntConst(z3.mkStringSymbol(v1)),z3.mkIntConst(z3.mkStringSymbol(v2)))
    case FunAssgn(v1, v2, f) => throw new RuntimeException("General functions not handled with Z3")
    case DataAssgn(v, d) => //throw new RuntimeException("General data assignments not handled with Z3")
      if (d.isInstanceOf[Int])
        z3.mkEq(z3.mkIntConst(z3.mkStringSymbol(v)),z3.mkInt(d.asInstanceOf[Int],z3.mkIntSort()))
      else
        throw new RuntimeException("General data assignments not handled with Z3")
    case Seq(Nil) => z3.mkTrue()
    case Seq(x::xs) => z3.mkAnd(gc2z3(x,z3),gc2z3(Seq(xs),z3))
  }




  def main(args:Array[String]) {
    val cfg = new Z3Config("MODEL" -> true) // required if you plan to query models of satisfiable constraints
    val z3 = new Z3Context(cfg)

    val bool = z3.mkBoolSort()
    val v1 = z3.mkBoolConst(z3.mkStringSymbol("v1v"))
    val v3 = z3.mkConst(z3.mkStringSymbol("v1v"),bool)
    val v2 = z3.mkConst(z3.mkStringSymbol("v2v"),bool)


    // v1 /\ v2
    val const_old = z3.mkAnd(z3.mkNot(z3.mkAnd(v1,v2)),v1)
    val const = gc2z3( Var("F$x1") --> ((Var("F$x2") and IntPred("D$x2",new Even)) and IntAssgn("D$x1",4)) ,z3)



    z3.assertCnstr(const)

    z3.checkAndGetModel() match {
      case (None, _) => println("Z3 failed. The reason is: " + z3.getSearchFailure.message)
      case (Some(false), _) => println("Unsat.")
      case (Some(true), model) => {
//        for (c <- model.getModelConstants) println ("const "+c)
//        println("v1: " + model.evalAs[Boolean](v1))
//        println("v2: " + model.evalAs[Boolean](v2))
//        println("v3: " + model.evalAs[Boolean](v3))
        println("model:\n"+model)
//        model.delete
      }
    }

//    for (d<-z3.) println("decl: "+d)


  }
}
