package common.beh.choco

import choco.kernel.model.variables.integer.IntegerVariable
import choco.Choco
import choco.kernel.model.constraints.{Constraint => ChocoConstr}

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 29/02/12
 * Time: 16:49
 * To change this template use File | Settings | File Templates.
 */


sealed abstract class ConstrBuilder {

  type VarMap = Map[String, IntegerVariable]

  def toChoco: (VarMap, ChocoConstr) = {
    val v = optimiseEqVars(Map(),true)
    toChocoAux(v)
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
  
//  def toChoco(vars: VarMap): (VarMap, ChocoConstr) = {
//    val v = optimizeEqVars(vars)
//    toChoco(v,)
//  }


  

  private def toChocoAux(vars: VarMap): (VarMap, ChocoConstr) = this match {
    case Var(name: String) => {
      val (m, v) = ConstrBuilder.getVar(vars, name)
      (m, Choco.eq(v, 1))
    }
    case VarEq(n1: String, n2: String) => {
      val (vars2, v1) = ConstrBuilder.getVar(vars,  n1)
      val (vars3, v2) = ConstrBuilder.getVar(vars2, n2)
      (vars3, Choco.eq(v1, v2))
    }
    case Neg(c: ConstrBuilder) => (vars, Choco.not(c.toChocoAux(vars) _2))
    case And(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars)
      val (m2, v2) = c2.toChocoAux(m1)
      (m2, Choco.and(v1, v2))
    }
    case Or(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars)
      val (m2, v2) = c2.toChocoAux(m1)
      (m2, Choco.or(v1, v2))
    }
    case Impl(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars)
      val (m2, v2) = c2.toChocoAux(m1)
      (m2, Choco.implies(v1, v2))
    }
    case Equiv(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars)
      val (m2, v2) = c2.toChocoAux(m1)
      (m2, Choco.and(Choco.implies(v1, v2), Choco.implies(v2, v1)))
    }
    case FalseC => (vars, Choco.FALSE)
    case TrueC => (vars, Choco.TRUE)
    // TODO: complete cases
//    case _ => (vars, Choco.TRUE)
  }


}

object ConstrBuilder {
  type VarMap = Map[String, IntegerVariable]

  def getVar(m: VarMap, name: String): (VarMap, IntegerVariable) = {
    if (m contains name)
      (m, m(name))
    else {
      val v = Choco.makeBooleanVar(name)
      (m + (name -> v), v)
    }
  }
  
  def toChoco(cs: Iterable[ConstrBuilder]): (VarMap,Iterable[ChocoConstr]) = {
    var varmap: VarMap = Map()
    var res:Set[ChocoConstr] = Set()
    for (c <- cs)
      varmap = c.optimiseEqVars(varmap,true)
    for (c <- cs) {
      val pair = c.toChocoAux(varmap)
      res += pair._2
      varmap = pair._1
    }
    (varmap,res)
  }
  
  def flowVar(x:String,uid:Int): String = uid + "$F$" +x
  def dataVar(x:String,uid:Int): String = uid + "$D$" +x
}

case class Var(name: String) extends ConstrBuilder

case class VarEq(v1: String, v2: String) extends ConstrBuilder

case class Neg(c: ConstrBuilder) extends ConstrBuilder

case class And(c1: ConstrBuilder, c2: ConstrBuilder) extends ConstrBuilder

case class Or(c1: ConstrBuilder, c2: ConstrBuilder) extends ConstrBuilder

case class Impl(c1: ConstrBuilder, c2: ConstrBuilder) extends ConstrBuilder

case class Equiv(c1: ConstrBuilder, c2: ConstrBuilder) extends ConstrBuilder

case object FalseC extends ConstrBuilder
case object TrueC  extends ConstrBuilder
