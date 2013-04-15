package common.choco

import choco.kernel.model.variables.integer.{IntegerExpressionVariable, IntegerVariable}
import choco.Choco
import choco.kernel.model.constraints.{Constraint => ChocoConstr}
import genericconstraints.{PredManager}
import scala.collection.JavaConversions._
import common.{Buffer, Predicate, Utils, Function}
import scala.collection.mutable

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
    toChoco(Map())
  }

  def toChoco(vm: VarMap) : (VarMap, ChocoConstr) = {
    val v = optimiseEqVars(vm,true)
    toChocoAux(v,new Buffer,true,true)
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

//    // continue traversal until ChocoConstraints are found in the leafs
//    case Or(c1, c2)  => c2.optimiseEqVars(c1.optimiseEqVars(vars,false),false)
//    case Impl(c1, c2)  => c2.optimiseEqVars(c1.optimiseEqVars(vars,false),false)
//    case Equiv(c1, c2)  => c2.optimiseEqVars(c1.optimiseEqVars(vars,false),false)
//
//    // ASSUMPTION: Injected choco contraints do not intersect variables with other choco constraints!
//    case ChocoConstraint(c) =>
//      val it = c.getVariableIterator
//      var vars2 = vars
//      while (it.hasNext) {
//        val v = it.next()
////        if (vars contains v.getName)
//        vars2 = ConstrBuilder.replaceVar(vars2,v.asInstanceOf[IntegerVariable])
//      }
//      vars2

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
    case LazyPred(v1,v2,v3,d,p,fs) => Set(v1,v2,v3)
    case FalseC => Set()
    case TrueC  => Set()
  }


//  def toChoco(vars: VarMap): (VarMap, ChocoConstr) = {
//    val v = optimizeEqVars(vars)
//    toChoco(v,)
//  }


  

  private def toChocoAux(vars: VarMap,buf: Buffer,pos:Boolean, top:Boolean): (VarMap, ChocoConstr) = this match {
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
      val (m,v) = c.toChocoAux(vars,buf,!pos,top)
      (m,Choco.not(v))

    case And(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars,buf,pos,top)
      val (m2, v2) = c2.toChocoAux(m1,buf,pos,top)
      (m2, Choco.and(v1, v2))
    }
    case Or(c1: ConstrBuilder, c2: ConstrBuilder) => {
//      val (m1, v1) = c1.toChocoAux(vars,buf,false,false)
//      val (m2, v2) = c2.toChocoAux(m1,buf,false,false)
//      (m2, Choco.or(v1, v2))
      val ors = mutable.Set[ChocoConstr]()
      val m1 = toChocoAuxOrs(this,vars,buf,ors)
      if (ors.isEmpty) (m1,Choco.TRUE)
      else {
        var res = ors.head
          for (c <- ors.tail)
            res = Choco.or(c,res)
        (m1,res)
      }
    }
    case Impl(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars,buf,false,false)
      val (m2, v2) = c2.toChocoAux(m1,buf,false,false)
      (m2, Choco.implies(v1, v2))
    }
    case Equiv(c1: ConstrBuilder, c2: ConstrBuilder) => {
      val (m1, v1) = c1.toChocoAux(vars,buf,false,false)
      val (m2, v2) = c2.toChocoAux(m1,buf,false,false)
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
    case FunAssgn(v1, v2, f) =>
      val (vars2, nv1) = ConstrBuilder.getVar(vars, v1)
      val (vars3, nv2) = ConstrBuilder.getVar(vars2, v2)
      val c = Choco.eq(nv1, f(nv2))
      (vars3, c)
    case LazyPred(v1,v2,v3,d,p,fs) =>
      val (vars2, nv1) = ConstrBuilder.getVar(vars , v1)
      val (vars3, nv2) = ConstrBuilder.getVar(vars2, v2)
      val (vars4, nv3) = ConstrBuilder.getVar(vars3, v3)
      val c = PredManager.genConstr(nv1, nv2, nv3, d, buf, p, fs) // c is a ComponentConstraint
      (vars4,c)

    case FalseC => (vars, Choco.FALSE)
    case TrueC => (vars, Choco.TRUE)
  }

  // Optimisation to avoid stack overflow on the sequence of long Ors (at least a variable with flow).
  private def toChocoAuxOrs(c: ConstrBuilder,vars: VarMap,buf: Buffer,cs:mutable.Set[ChocoConstr]): VarMap = c match {
    case Or(c1,Or(c2,c3)) =>
      val m1 = toChocoAuxOrs(c1,vars,buf,cs)
      val m2 = toChocoAuxOrs(c2,m1,buf,cs)
      toChocoAuxOrs(c3,m2,buf,cs)
    case Or(Or(c1,c2),c3) =>
      val m1 = toChocoAuxOrs(c1,vars,buf,cs)
      val m2 = toChocoAuxOrs(c2,m1,buf,cs)
      toChocoAuxOrs(c3,m2,buf,cs)
    case Or(c1,c2) =>
      val m1 = toChocoAuxOrs(c1,vars,buf,cs)
      toChocoAuxOrs(c2,m1,buf,cs)
    case _: ConstrBuilder =>
      val (vars2,c2) = c.toChocoAux(vars,buf,false,false)
      cs += c2
      vars2
  }


  override def toString = this match {
    case Var(name) =>  name
    case VarEq(v1, v2) =>  v1 +" = "+ v2
    case Neg(c) => "¬("+c+")"
    case And(c1, c2) => "("+c1+") & ("+c2+")"
    case Or(c1, c2) =>  "("+c1+") | ("+c2+")"
    case Impl(c1, c2) => "("+c1+") --> ("+c2+")"
    case Equiv(c1, c2) => "("+c1+") <-> ("+c2+")"
    case FlowPred(p, v) => v
    case DataAssgn(v, d) => v+" := "+d
    case FunAssgn(v1, v2, f) => v1+" := "+f+"("+v2+")"
    case LazyPred(v1, v2, v3, d, p, fs) => "LAZY("+v1+","+v2+","+v3+","+d+","+p+","+fs.mkString("-")+")"
    case FalseC => "F"
    case TrueC => "T"
  }

}

object ConstrBuilder {
  type VarMap = Map[String, IntegerVariable]

  /**
   * Retrieves an IntegerVariable from a map if it exists, or creates one if it does not exist, updating the map.
   * @param m map from variable names (Strings) to Choco variables (IntegerVariables)
   * @param name of the variable (String)
   * @return Choco variable (IntegerVariable) for the given variable name
   */
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

//  def replaceVar(vars: VarMap, v: IntegerVariable): VarMap = {
//    var res: VarMap = Map()
//    val vname = v.getName
//    if (vars contains vname) {
//      val oldvar = vars(vname)
//      for ((x,y) <- vars)
//        if (y == oldvar) res += x -> v
//        else             res += x -> y
//    }
//    else
//      res = vars + (vname -> v)
//    res
//  }

//  /**
//   * Builds a collection of Choco constraints from a collection of constraint builders
//   * @param cs collection of constraint builders
//   * @return equivalent Choco constraint
//   */
//  def toChoco(cs: Iterable[ConstrBuilder]): (VarMap,Iterable[ChocoConstr]) = {
//    toChoco(cs,new Buffer)
//  }

  /**
   * Same as toChoco but parameterised in the buffer used by lazy constraints.
   * @param cs collection of contraint builders
   * @param buf buffer of already calculated functions and predicates in lazy constraints
   * @return Choco constraint
   */
  def toChoco(cs: Iterable[ConstrBuilder], buf: Buffer): (VarMap,Iterable[ChocoConstr]) = {
    var varmap: VarMap = Map()
    var res:Set[ChocoConstr] = Set()
    for (c <- cs) {
      varmap = c.optimiseEqVars(varmap,true)
    }
//    val buf = new Buffer
    for (c <- cs) {
      val pair = c.toChocoAux(varmap,buf,true,true)
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
case class LazyPred(v1:String, v2:String, v3:String, d:Any, p:Predicate, fs: List[Function]) extends ConstrBuilder
case object FalseC extends ConstrBuilder
case object TrueC  extends ConstrBuilder
