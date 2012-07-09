package common.beh.choco

import choco.kernel.model.variables.integer.{IntegerExpressionVariable, IntegerVariable}
import choco.Choco
import choco.kernel.model.constraints.{Constraint => ChocoConstr}
import common.beh.Utils

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 29/02/12
 * Time: 16:49
 * To change this template use File | Settings | File Templates.
 */


sealed abstract class ConstrBuilder {
  type VarMap = Map[String, IntegerVariable]

  def -->(c:ConstrBuilder) = Impl(this,c)
  def <->(c:ConstrBuilder) = Equiv(this,c)
  def and(c:ConstrBuilder) = And(this,c)
  def or(c:ConstrBuilder) = Or(this,c)

  def toChoco: (VarMap, ChocoConstr) = {
    val v = optimiseEqVars(Map(),true)
    toChocoAux(v,true,true)
  }

  private def optimiseEqVars(vars: VarMap,b:Boolean): VarMap = this match {
    case VarEq(n1: String, n2: String) => {
      if (!b) vars
      else {
        if (vars contains n1)
          vars + (n2 -> vars(n1))
        else {
          val (vars2,ivar) = ConstrBuilder.getVar(vars,n2)
          vars2 + (n1 -> ivar)
        }
      }
    }
    case Neg(c) => c.optimiseEqVars(vars,!b)
    case And(c1, c2)  => c2.optimiseEqVars(c1.optimiseEqVars(vars,b),b)
//    case Or(c1, c2)  => c2.optimiseEqVars(c1.optimiseEqVars(vars,b),b)
//    case Impl(c1, c2)  => c2.optimiseEqVars(c1.optimiseEqVars(vars))
//    case Equiv(c1, c2)  => c2.optimiseEqVars(c1.optimiseEqVars(vars))
    case _ => vars
  }

  def getVars: Iterable[String] = this match {
    case Var(name)     => Set(name)
    case VarEq(v1, v2) => Set(v1,v2)
    case Neg(c)        => c.getVars
    case And(c1, c2)   => c1.getVars ++ c2.getVars
    case Or(c1, c2)    => c1.getVars ++ c2.getVars
    case Impl(c1, c2)  => c1.getVars ++ c2.getVars
    case Equiv(c1, c2) => c1.getVars ++ c2.getVars
    case FlowPred(p, v) => Set(v)
    case DataAssgn(v, d)=> Set(v)
    case FunAssgn(v1, v2, f) => Set(v1,v2)
    case FalseC => Set()
    case TrueC  => Set()
  }


//  def toChoco(vars: VarMap): (VarMap, ChocoConstr) = {
//    val v = optimizeEqVars(vars)
//    toChoco(v,)
//  }


  

  private def toChocoAux(vars: VarMap,pos:Boolean,top:Boolean): (VarMap, ChocoConstr) = this match {
    case Var(name: String) =>
      val (m, v) = ConstrBuilder.getVar(vars, name)
      (m, Choco.eq(v, 1))

    case VarEq(n1: String, n2: String) =>
      if (pos && top) (vars,Choco.TRUE)
      else {
        val (vars2, v1) = ConstrBuilder.getVar(vars, n1)
        val (vars3, v2) = ConstrBuilder.getVar(vars2, n2)
        (vars3, Choco.eq(v1, v2))
      }

    case Neg(c: ConstrBuilder) => //(vars, Choco.not(c.toChocoAux(vars,!b)._2))
      val (m,v) = c.toChocoAux(vars,!pos,top)
      (m,Choco.not(v))

    case And(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars,pos,top)
      val (m2, v2) = c2.toChocoAux(m1,pos,top)
      (m2, Choco.and(v1, v2))
    }
    case Or(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars,false,false)
      val (m2, v2) = c2.toChocoAux(m1,false,false)
      (m2, Choco.or(v1, v2))
    }
    case Impl(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars,false,false)
      val (m2, v2) = c2.toChocoAux(m1,false,false)
      (m2, Choco.implies(v1, v2))
    }
    case Equiv(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars,false,false)
      val (m2, v2) = c2.toChocoAux(m1,false,false)
      (m2, Choco.and(Choco.implies(v1, v2), Choco.implies(v2, v1)))
    }
    case FlowPred(p: (IntegerVariable => ChocoConstr), v: String) => {
      val (vars2, v1) = ConstrBuilder.getVar(vars, v)
      val c = p(v1)
      (vars2,c)
    }
    case DataAssgn(v: String, d:Int) => {
      val (vars2, v1) = ConstrBuilder.getVar(vars, v)
      val c = Choco.eq(v1,d)
      (vars2,c)
    }
    case FunAssgn(v1, v2, f) => {
      val (vars2, nv1) = ConstrBuilder.getVar(vars , v1)
      val (vars3, nv2) = ConstrBuilder.getVar(vars2, v2)
      val c = Choco.eq(nv1, f(nv2))
      (vars3,c)
    }
    case FalseC => (vars, Choco.FALSE)
    case TrueC => (vars, Choco.TRUE)
  }


}

object ConstrBuilder {
  type VarMap = Map[String, IntegerVariable]

  def getVar(m: VarMap, name: String): (VarMap, IntegerVariable) = {
    if (m contains name)
      (m, m(name))
    else if (Utils.isFlowVar(name)) {
      val v = Choco.makeBooleanVar(name)
      (m + (name -> v), v)
    }
    else if (Utils.isPredVar(name)) {
      val v = Choco.makeBooleanVar(name)
      (m + (name -> v), v)
    }
    else {
      val v = Choco.makeIntVar(name)
      (m + (name -> v), v)
    }
  }
  
  def toChoco(cs: Iterable[ConstrBuilder]): (VarMap,Iterable[ChocoConstr]) = {
    var varmap: VarMap = Map()
    var res:Set[ChocoConstr] = Set()
    for (c <- cs) {
      varmap = c.optimiseEqVars(varmap,true)
    }
    for (c <- cs) {
      val pair = c.toChocoAux(varmap,true,true)
      res += pair._2
      varmap = pair._1
    }
    (varmap,res)
  }
}

case class Var(name: String) extends ConstrBuilder

case class VarEq(v1: String, v2: String) extends ConstrBuilder

case class Neg(c: ConstrBuilder) extends ConstrBuilder

case class And(c1: ConstrBuilder, c2: ConstrBuilder) extends ConstrBuilder

case class Or(c1: ConstrBuilder, c2: ConstrBuilder) extends ConstrBuilder

case class Impl(c1: ConstrBuilder, c2: ConstrBuilder) extends ConstrBuilder

case class Equiv(c1: ConstrBuilder, c2: ConstrBuilder) extends ConstrBuilder

case class FlowPred(p: IntegerVariable => ChocoConstr, v: String) extends ConstrBuilder

case class DataAssgn(v: String, d: Int) extends ConstrBuilder

case class FunAssgn(v1: String, v2: String, f: IntegerVariable => IntegerExpressionVariable) extends ConstrBuilder

case object FalseC extends ConstrBuilder
case object TrueC  extends ConstrBuilder
