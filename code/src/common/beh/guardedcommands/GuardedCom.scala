package common.beh.guardedcommands

import common.beh.choco.dataconnectors.Predicate
import collection.mutable.{Set => MutSet, Map => MutMap}
import collection.immutable.{Set => ImSet}
import common.beh.Utils._
import common.beh.choco.{TrueC, ConstrBuilder}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:25
 * To change this template use File | Settings | File Templates.
 */

case class GuardedCom(g:Guard, st: Statement) {
  def fv:Set[String] = g.fv ++ st.fv
  def da:DomainAbst = g.da + st.da
  def afv(dom: DomainAbst) :Set[String] = {
    var res = Set[String]()
//    val dx = da // PROBLEM: da of the single command, not of the full expression  - solved
    for (v <- fv) {
      if (!isFlowVar(v)) {
        val dv = dom.domain(v) // v -> Set(P1,P2,P3)
        for (p <- dv)
          res += predVar(v,p)
      }
      else {
//        println("Adding flow var "+v)
        res += v
      }
    }
    res
  }
  def toCNF(vars: MutMap[String,Int],da: DomainAbst): CNF.Core =  {
    // recalculate "not g or st"
    var list: List[Array[Int]] = List()
    for (cl1 <- Neg(g).toCNF(vars,da); cl2 <- st.toCNF(vars,da))
      list = (cl1 ++ cl2) :: list
    list
  }

  def toConstrBuilder(da: DomainAbst): ConstrBuilder = {
    g.toConstrBuilder --> st.toConstrBuilder(da)
  }

  def partialEval(sol:GCBoolSolution): PEval = { //(Map[String,Int], Map[String,String]) = {
    if (g.eval(sol))
      st.partialEval(sol)
    else new PEval(Map(),Map())
  }
}


// what I still need:
// 1) free vars (and a map from numbers to them) - DONE
// 2) constraints for Dx - pair of sequences (order) and restrictions (x -> preds) - DONE
// 3) build DA from a GC
// 3) predicate abstractions Dx for each x
// 4) CNF based on Dx

abstract sealed class Guard {
  def and(e: Guard) = And(this,e)
  def or(e: Guard) = Or(this,e)
  def -->(e: Guard) = Impl(this,e)
  def -->(e: Statement) = GuardedCom(this,e)
  def <->(e: Guard) = Equiv(this,e)

  def fv: Set[String] = this match {
    case Var(name) => Set(name)
    case Pred(p, v) => Set(v)
    case And(g1, g2) => g1.fv ++ g2.fv
    case Or(g1, g2) => g1.fv ++ g2.fv
    case Neg(g) => g.fv
    case Impl(g1, g2) => g1.fv ++ g2.fv
    case Equiv(g1, g2) => g1.fv ++ g2.fv
    case True => Set()
  }

  def da: DomainAbst = this match {
    case Var(name) =>  DomainAbst()
    case Pred(p, v) => DomainAbst(v,p)
    case And(g1, g2) => g1.da + g2.da
    case Or(g1, g2) => g1.da + g2.da
    case Neg(g) => g.da
    case Impl(g1, g2) => g1.da + g2.da
    case Equiv(g1, g2) => g1.da + g2.da
    case True => DomainAbst()
  }

  def toCNF(vars: MutMap[String,Int],da: DomainAbst): CNF.Core = this match {
    case Impl(e1,e2) => (Neg(e1) or e2).toCNF(vars,da)
    case Neg(Impl(e1,e2)) => (Neg(e2) and e1).toCNF(vars,da)
    case Equiv(e1,e2) => ((e1 --> e2) and (e2 -->e1)).toCNF(vars,da)
    case Neg(Equiv(e1,e2)) => (Neg(e1 --> e2) or Neg(e2 --> e1)).toCNF(vars,da)
    case And(c1,c2) => c1.toCNF(vars,da) ++ c2.toCNF(vars,da)
    case Neg(And(e1,e2)) => (Neg(e1) or Neg(e2)).toCNF(vars,da)
    case Or(c1,c2) => {
      var list: List[Array[Int]] = List()
      for (cl1 <- c1.toCNF(vars,da); cl2 <- c2.toCNF(vars,da))
        list = (cl1 ++ cl2) :: list
      list
    }
    case Neg(Or(e1,e2)) => (Neg(e1) and Neg(e2)).toCNF(vars,da)
    case Var(a) => List(Array(vars(a)))
    case Neg(Var(a)) => List(Array(vars(a)*(-1)))
    case Pred(p, v) => Var(predVar(v,p)).toCNF(vars,da)
//      if (da.domain(v) contains p) Var(predVar(v,p)).toCNF(vars,da)
//      else List(Array())
    case Neg(Pred(p, v)) => List(Array(vars(predVar(v,p))*(-1)))
//      Neg(Var(predVar(v,p))).toCNF(vars,da)
    case True => List()
    case Neg(True) => List(Array())
    case Neg(Neg(a)) => a.toCNF(vars,da)
  }

  def toConstrBuilder: ConstrBuilder = this match {
    case Var(name) => common.beh.choco.Var(name)
    case Pred(p, v) => common.beh.choco.Var(predVar(v,p))//common.beh.choco.FlowPred(p.choPred,v)
    case And(g1, g2) => g1.toConstrBuilder and g2.toConstrBuilder
    case Or(g1, g2) =>  g1.toConstrBuilder or  g2.toConstrBuilder
    case Neg(g) => common.beh.choco.Neg(g.toConstrBuilder)
    case Impl(g1, g2) => g1.toConstrBuilder --> g2.toConstrBuilder
    case Equiv(g1, g2) => g1.toConstrBuilder <-> g2.toConstrBuilder
    case True => common.beh.choco.TrueC
  }

  def eval(sol: GCBoolSolution): Boolean = this match {
    case Var(name) => sol(name)
    case Pred(p, v) => sol(predVar(v,p))
    case And(g1, g2) => g1.eval(sol) && g1.eval(sol)
    case Or(g1, g2) => g1.eval(sol) || g1.eval(sol)
    case Neg(g) => !g.eval(sol)
    case Impl(g1, g2) => !g1.eval(sol) || g2.eval(sol)
    case Equiv(g1, g2) => ((g1-->g2) and (g2-->g1)).eval(sol)
    case True => true
  }

}

abstract sealed class Statement {
  def fv: Set[String] = this match {
    case SGuard(g) => g.fv
    case DataAssgn(v, d) => Set(v)
    case VarAssgn(v1, v2) => Set(v1,v2)
    case Seq(Nil) => Set()
    case Seq(s::ss) => s.fv ++ Seq(ss).fv
  }

  def da: DomainAbst = this match {
    case SGuard(g) => g.da
    case DataAssgn(v, d) => DomainAbst()
    case VarAssgn(v1, v2) => DomainAbst(v2 -> v1)
    case Seq(Nil) => DomainAbst()
    case Seq(s::ss) => s.da + Seq(ss).da
  }

  def toCNF(vars: MutMap[String,Int],da: DomainAbst): CNF.Core = this match {
    case SGuard(g) => g.toCNF(vars,da)
    case DataAssgn(v, d) =>
//      println("converting data assgnm '"+v+" := "+d+"'.")
      var res:List[Int] = List()
      val dom = da.domain(v)
      for (pred <- dom)
        if (pred.funPred(d))
          res ::= vars(predVar(v,pred))      // ^a := d --> ^a_p not in vars...
        else
          res ::= vars(predVar(v,pred)) * (-1)
//      println("got array "+res.mkString("[",",","]"))
      res.map(Array(_))
//      List(res.toArray)
    case VarAssgn(v1, v2) =>
//      println("converting var eq of "+v1+" and "+v2)
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: CNF.Core = List()
      for (pred <- d1)
        if (d2 contains pred) {
          val t =  (Var(predVar(v1,pred)) <-> Var(predVar(v2,pred))).toCNF(vars,da)
//          println("converting "+v1+" <-> "+v2+" for "+pred+" - "+ t.map(_.mkString(",")).mkString("["," ; ","]"))
          res :::= t
        }
//      println("conversion: "+res.map(_.mkString(",")).mkString("["," ; ","]"))
      res
    case Seq(Nil) => List()
    case Seq(s::ss) => s.toCNF(vars,da) ++ Seq(ss).toCNF(vars,da)
  }

  def toConstrBuilder(da: DomainAbst): ConstrBuilder = this match {
    case SGuard(g) => g.toConstrBuilder
    case DataAssgn(v, d) => //common.beh.choco.DataAssgn(v,d)
      var res:ConstrBuilder = TrueC
      val dom = da.domain(v)
      for (pred <- dom)
        if (pred.funPred(d))
          res = res and common.beh.choco.Var(predVar(v,pred))
        else
          res = res and common.beh.choco.Neg(common.beh.choco.Var(predVar(v,pred)))
      //      println("got array "+res.mkString("[",",","]"))
      res
    case VarAssgn(v1, v2) =>
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: ConstrBuilder= TrueC
      for (pred <- d1)
        if (d2 contains pred) {
          val t = common.beh.choco.VarEq(predVar(v1,pred),predVar(v2,pred))
                  //(common.beh.choco.Var(predVar(v1,pred)) <-> common.beh.choco.Var(predVar(v2,pred)))
          res = res and t
        }
      res
    case Seq(Nil) => common.beh.choco.TrueC
    case Seq(s::ss) => s.toConstrBuilder(da) and Seq(ss).toConstrBuilder(da)
  }

  def partialEval(sol: GCBoolSolution): PEval = this match {
    case SGuard(g) => new PEval(Map(),Map())
    case DataAssgn(v, d) => new PEval(Map(v -> d),Map())
    case VarAssgn(v1, v2) => new PEval(Map(),Map(v1 -> ImSet(v2)))
    case Seq(Nil) => new PEval(Map(),Map())
    case Seq(s::ss) =>
      val x1 = s.partialEval(sol)
      val x2 = Seq(ss).partialEval(sol)
      x1 ++ x2
      //(x1._1 ++ x2._1, x1._2 ++ x2._2)
  }

}

case class Var(name: String) extends Guard
case class Pred(p: Predicate, v:String) extends Guard
case class And(g1: Guard, g2: Guard) extends Guard
case class Or(g1: Guard, g2: Guard) extends Guard
case class Neg(g: Guard) extends Guard
case class Impl(g1: Guard, g2: Guard) extends Guard
case class Equiv(g1: Guard, g2: Guard) extends Guard
case object True extends Guard

case class SGuard(g: Guard) extends Statement
case class DataAssgn(v: String, d: Int) extends Statement
case class VarAssgn(v1: String, v2: String) extends Statement
case class Seq(sts: List[Statement]) extends Statement


// CNF
object CNF {
  type Core = List[Array[Int]]

  def fv(cnf: CNF.Core): Iterable[Int] = {
    var s: MutSet[Int] = MutSet()
    for (l <- cnf; v <- l)
      if (v<0) s += (v * (-1))
      else     s+= v
    s
  }

  def not(s:String) = (-1) * s.hashCode
  def v(s:String) = s.hashCode
}

class CNF(val cnf:CNF.Core, val vars: Array[String])


