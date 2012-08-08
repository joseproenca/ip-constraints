package common.beh.guardedcommands

import common.beh.{UnPredicate, UnFunction, IntPredicate, IntFunction}
import collection.mutable.{Set => MutSet, Map => MutMap}
import collection.immutable.{Set => ImSet}
import common.beh.Utils._
import common.beh.choco.{LazyPred, TrueC, ConstrBuilder}

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
//        println(" ** "+v+" - "+dv.mkString(","))
        for ((p,fs) <- dv)
          res += predVar(v,p,fs)
      }
      else {
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

  def toConstrBuilder: ConstrBuilder = {
    g.toConstrBuilder --> st.toConstrBuilder
  }

  def toBoolConstrBuilder(da: DomainAbst): ConstrBuilder = {
    g.toBoolConstrBuilder --> st.toBoolConstrBuilder(da)
  }

  def partialEval(sol:GCBoolSolution): PEval = { //(Map[String,Int], Map[String,String]) = {
    if (g.eval(sol))
      st.partialEval(sol)
    else new PEval(Map(),Map(),Map())
  }

  override def toString = g + " --> " + st

}


// what I still need:
// 1) free vars (and a map from numbers to them) - DONE
// 2) constraints for Dx - pair of sequences (order) and restrictions (x -> preds) - DONE
// 3) build DA from a GC
// 3) predicate abstractions Dx for each x
// 4) CNF based on Dx

abstract sealed class Guard extends Statement{
  def and(e: Guard) = And(this,e)
  def or(e: Guard) = Or(this,e)
  def ->(e: Guard) = Impl(this,e)
  def -->(e: Statement) = GuardedCom(this,e)
  def <->(e: Guard) = Equiv(this,e)

  override def fv: Set[String] = this match {
    case Var(name) => Set(name)
    case IntPred(v, p) => Set(v)
    case Pred(v, p) => Set(v)
    case And(g1, g2) => g1.fv ++ g2.fv
    case Or(g1, g2) => g1.fv ++ g2.fv
    case Neg(g) => g.fv
    case Impl(g1, g2) => g1.fv ++ g2.fv
    case Equiv(g1, g2) => g1.fv ++ g2.fv
    case True => Set()
    case s => super.fv
  }

  override def da: DomainAbst = this match {
    case Var(name) =>  DomainAbst()
    case IntPred(v, p) => DomainAbst(v,p)
    case Pred(v, p) => DomainAbst(v,p)
    case And(g1, g2) => g1.da + g2.da
    case Or(g1, g2) => g1.da + g2.da
    case Neg(g) => g.da
    case Impl(g1, g2) => g1.da + g2.da
    case Equiv(g1, g2) => g1.da + g2.da
    case True => DomainAbst()
    case s => super.da
  }

  override def toCNF(vars: MutMap[String,Int],da: DomainAbst): CNF.Core = this match {
    case Impl(e1,e2) => (Neg(e1) or e2).toCNF(vars,da)
    case Neg(Impl(e1,e2)) => (Neg(e2) and e1).toCNF(vars,da)
    case Equiv(e1,e2) => ((e1 -> e2) and (e2 -> e1)).toCNF(vars,da)
    case Neg(Equiv(e1,e2)) => (Neg(e1 -> e2) or Neg(e2 -> e1)).toCNF(vars,da)
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
    case IntPred(v, p) => Var(predVar(v,p,List())).toCNF(vars,da)
//      if (da.domain(v) contains p) Var(predVar(v,p)).toCNF(vars,da)
//      else List(Array())
    case Neg(IntPred(v, p)) => List(Array(vars(predVar(v,p,List()))*(-1)))
//      Neg(Var(predVar(v,p))).toCNF(vars,da)
    case Pred(v, p) => Var(predVar(v,p,List())).toCNF(vars,da)
    case Neg(Pred(v, p)) => List(Array(vars(predVar(v,p,List()))*(-1)))
    case True => List()
    case Neg(True) => List(Array())
    case Neg(Neg(a)) => a.toCNF(vars,da)
    case s => super.toCNF(vars,da)
  }

  override def toConstrBuilder: ConstrBuilder = this match {
    case Var(name) => common.beh.choco.Var(name)
    case IntPred(v, p) => common.beh.choco.FlowPred(p.choPred,v) // THIS is the difference with Bool below.
    case Pred(v, p) => p match {
      case intp: IntPredicate => common.beh.choco.FlowPred(intp.choPred,v)
      case _ => throw new Exception("General predicates have no associated choco constraint")
    }
    case And(g1, g2) => g1.toConstrBuilder and g2.toConstrBuilder
    case Or(g1, g2) => g1.toConstrBuilder or g2.toConstrBuilder
    case Neg(g) => common.beh.choco.Neg(g.toConstrBuilder)
    case Impl(g1, g2) => g1.toBoolConstrBuilder --> g2.toBoolConstrBuilder
    case Equiv(g1, g2) => g1.toBoolConstrBuilder <-> g2.toBoolConstrBuilder
    case True => common.beh.choco.TrueC
    case s => super.toConstrBuilder
  }

  def toBoolConstrBuilder: ConstrBuilder = this match {
    case Var(name) => common.beh.choco.Var(name)
    case IntPred(v, p) => common.beh.choco.Var(predVar(v,p,List()))//common.beh.choco.FlowPred(p.choPred,v)
    case Pred(v, p) => common.beh.choco.Var(predVar(v,p,List()))
    case And(g1, g2) => g1.toBoolConstrBuilder and g2.toBoolConstrBuilder
    case Or(g1, g2) =>  g1.toBoolConstrBuilder or  g2.toBoolConstrBuilder
    case Neg(g) => common.beh.choco.Neg(g.toBoolConstrBuilder)
    case Impl(g1, g2) => g1.toBoolConstrBuilder --> g2.toBoolConstrBuilder
    case Equiv(g1, g2) => g1.toBoolConstrBuilder <-> g2.toBoolConstrBuilder
    case True => common.beh.choco.TrueC
  }

  def eval(sol: GCBoolSolution): Boolean = this match {
    case Var(name) => sol(name)
    case IntPred(v, p) => sol(predVar(v,p,List()))
    case Pred(v, p) => sol(predVar(v,p,List()))
    case And(g1, g2) => g1.eval(sol) && g1.eval(sol)
    case Or(g1, g2) => g1.eval(sol) || g1.eval(sol)
    case Neg(g) => !g.eval(sol)
    case Impl(g1, g2) => !g1.eval(sol) || g2.eval(sol)
    case Equiv(g1, g2) => ((g1->g2) and (g2->g1)).eval(sol)
    case True => true
  }

  override def toString = this match {
    case Var(name) => ppFlowVar(name)
    case IntPred(v, p) => p+"("+ppDataVar(v)+")"
    case Pred(v, p) => p+"("+ppDataVar(v)+")"
    case And(g1, g2) => g1.mbPar + " /\\ " + g2.mbPar
    case Or(g1, g2) => g1.mbPar + " \\/ " + g2.mbPar
    case Neg(g) => "¬"+g
    case Impl(g1, g2) => g1.mbPar + " -> " + g2.mbPar
    case Equiv(g1, g2) => g1.mbPar + " <-> " + g2.mbPar
    case True => "True"
  }

  def mbPar: String = this match {
    case g@And(g1, g2) => "(" + g + ")"
    case g@Or(g1, g2) => "(" + g + ")"
    case g@Impl(g1, g2) => "(" + g + ")"
    case g@Equiv(g1, g2) => "(" + g + ")"
    case x => x.toString
  }

}

abstract sealed class Statement {
  def and(s: Statement) = Seq(List(this,s))

  def fv: Set[String] = this match {
    case g: Guard => g.fv
    case IntAssgn(v, _) => Set(v)
    case DataAssgn(v, _) => Set(v)
    case VarAssgn(v1, v2) => Set(v1,v2)
    case FunAssgn(v1,v2,_) => Set(v1,v2)
    case Seq(Nil) => Set()
    case Seq(s::ss) => s.fv ++ Seq(ss).fv
//    case g: Guard => super.fv
  }

  def da: DomainAbst = this match {
    case g: Guard => g.da
    case IntAssgn(_,_) => DomainAbst()
    case DataAssgn(_,_) => DomainAbst()
    case VarAssgn(v1, v2) => DomainAbst(v2 -> v1)
    case Seq(Nil) => DomainAbst()
    case Seq(s::ss) => s.da + Seq(ss).da
    case FunAssgn(v1,v2,f) => DomainAbst(v2 -> v1) + DomainAbst(v1, f)
//    case g: Guard => super.da
  }

  def toCNF(vars: MutMap[String,Int],da: DomainAbst): CNF.Core = this match {
    case g: Guard => g.toCNF(vars,da)
    case IntAssgn(v,d)  => DataAssgn(v,d).toCNF(vars,da)
    case DataAssgn(v,d) =>
//      println("converting data assgnm '"+v+" := "+d+"'.")
      var res:List[Int] = List()
      val dom = da.domain(v)
      for ((pred,fs) <- dom) {
        // ALL PRECALCULATED (only in choco-Bool it is lazily calculated)
        var newd: Any = d
        for (f<-fs.reverse) newd = f.calculate(newd)
//        println("precomputing "+v+" for "+pred+" after "+fs+" ("+d+" -> "+newd+")")
        if (pred.check(newd)) {
          res ::= vars(predVar(v,pred,fs))
        }
        else
          res ::= vars(predVar(v,pred,fs)) * (-1)
      }
//      println("got array "+res.mkString("[",",","]"))
      res.map(Array(_))
//      List(res.toArray)

    case VarAssgn(v1, v2) =>
//      println("converting var eq of "+v1+" and "+v2)
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: CNF.Core = List()
      for ((pred,fs) <- d1)
        if (d2 contains (pred,fs)) {
          val t =  (Var(predVar(v1,pred,fs)) <-> Var(predVar(v2,pred,fs))).toCNF(vars,da)
//          println("converting "+v1+" <-> "+v2+" for "+pred+" - "+ t.map(_.mkString(",")).mkString("["," ; ","]"))
          res :::= t
        }
      res
    case FunAssgn(v1, v2, f) =>
      //      println("converting var eq of "+v1+" and "+v2)
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: CNF.Core = List()
//      println("adding := for new abst-vars "+FunAssgn(v1,v2,f)+"\ndomains:\n"+d1+"\n--\n"+d2)
      for ((pred,fs) <- d1)
        if (d2 contains (pred,f::fs)) {
//          println("adding "+predVar(v1,pred,fs)+" <-> "+predVar(v2,pred,f::fs))
          val t =  (Var(predVar(v1,pred,fs)) <-> Var(predVar(v2,pred,f::fs))).toCNF(vars,da)
          res :::= t
        }
    //      println("conversion: "+res.map(_.mkString(",")).mkString("["," ; ","]"))
      res

    case Seq(Nil) => List()
    case Seq(s::ss) => s.toCNF(vars,da) ++ Seq(ss).toCNF(vars,da)
//    case g: Guard => super.toCNF(vars,da)
  }

  def toConstrBuilder: ConstrBuilder = this match {
    case g: Guard => g.toConstrBuilder
    case IntAssgn(v, d) => common.beh.choco.DataAssgn(v,d)
    case DataAssgn(v, d) =>
      if (d.isInstanceOf[Int]) common.beh.choco.DataAssgn(v,d.asInstanceOf[Int])
      else throw new RuntimeException("General data assignments have no associated choco constraint")
    case VarAssgn(v1, v2) => common.beh.choco.VarEq(v1,v2)
    case FunAssgn(v1, v2, f) =>
      if (f.isInstanceOf[IntFunction]) common.beh.choco.FunAssgn(v1,v2,f.asInstanceOf[IntFunction].choFun)
      else throw new RuntimeException("General data functions have no associated choco constraint")
    case Seq(Nil) => common.beh.choco.TrueC
    case Seq(s::ss) => s.toConstrBuilder and Seq(ss).toConstrBuilder
//    case g: Guard => super.toConstrBuilder
  }

  def toBoolConstrBuilder(da: DomainAbst): ConstrBuilder = this match {
    case g: Guard => g.toBoolConstrBuilder
    case IntAssgn(v, d) => DataAssgn(v,d).toBoolConstrBuilder(da)
    case DataAssgn(v, d) => //common.beh.choco.DataAssgn(v,d)
      // INSTEAD OF CALCULATING, CREATE A LAZY CONSTRAINT!
      var res:ConstrBuilder = TrueC
      for ((pred,fs,xflow) <- da.domainWithEnd(v)) {

//        var newd = d
//        for (f<-fs.reverse) newd = f.calculate(newd)
//        if (pred.check(newd))
//          res = res and common.beh.choco.Var(predVar(v,pred,fs))
//        else
//          res = res and common.beh.choco.Neg(common.beh.choco.Var(predVar(v,pred,fs)))

        println("added LazyPred("+predVar(v,pred,fs)+","+data2flow(v)+","+data2flow(xflow)+","+fs+")")
        res = res and LazyPred(predVar(v,pred,fs),data2flow(v),data2flow(xflow),d,pred,fs)
      }
      res

    case VarAssgn(v1, v2) =>
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: ConstrBuilder= TrueC
      for ((pred,fs) <- d2)
        if (d1 contains (pred,fs)) {
          val t = common.beh.choco.VarEq(predVar(v1,pred,fs),predVar(v2,pred,fs))
          res = res and t
        }
      res

    case FunAssgn(v1,v2,f) =>
//      VarAssgn(v1,v2).toBoolConstrBuilder(da)
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: ConstrBuilder= TrueC
      for ((pred,fs) <- d1)
        if (d2 contains (pred,f::fs)) {
          val t = common.beh.choco.VarEq(predVar(v1,pred,fs),predVar(v2,pred,f::fs))
          res = res and t
        }
      res

    case Seq(Nil) => common.beh.choco.TrueC
    case Seq(s::ss) => s.toBoolConstrBuilder(da) and Seq(ss).toBoolConstrBuilder(da)
//    case g: Guard => super.toBoolConstrBuilder
  }

  def partialEval(sol: GCBoolSolution): PEval = this match {
    case g: Guard => new PEval(Map(),Map(),Map())
    case IntAssgn(v, d) => new PEval(Map(v -> d),Map(),Map())
    case DataAssgn(v, d) => new PEval(Map(v -> d),Map(),Map())
    case VarAssgn(v1, v2) => new PEval(Map(),Map(v2 -> ImSet(v1)),Map())
    // TODO: CHANGE PEval to split at x=f(y), and include this info in PEval
    case FunAssgn(v1, v2, f) => new PEval(Map(),Map(),Map(v2 -> ImSet((v1,f))))
    case Seq(Nil) => new PEval(Map(),Map(),Map())
    case Seq(s::ss) =>
      val x1 = s.partialEval(sol)
      val x2 = Seq(ss).partialEval(sol)
      x1 ++ x2
      //(x1._1 ++ x2._1, x1._2 ++ x2._2)
//    case g: Guard => new PEval(Map(),Map(),Map())
  }

  override def toString: String = this match {
    case g: Guard => g.toString
    case IntAssgn(v, d) => ppDataVar(v) + " := " + d
    case DataAssgn(v, d) => ppDataVar(v) + " := " + d
    case VarAssgn(v1, v2) => ppDataVar(v1) + " := " + ppDataVar(v2)
    case FunAssgn(v1, v2, f) => ppDataVar(v1) + " := " + f + "(" + ppDataVar(v2) + ")"
    case Seq(Nil) => ""
    case Seq(x::Nil) => x.toString
    case Seq(x::xs) => x + " ; " + xs
//    case g: Guard => super.toString
  }

}

/// GUARDS
case class Var(name: String) extends Guard
case class IntPred(v:String, p: IntPredicate) extends Guard
case class Pred(v:String, p:UnPredicate) extends Guard
case class And(g1: Guard, g2: Guard) extends Guard
case class Or(g1: Guard, g2: Guard) extends Guard
case class Neg(g: Guard) extends Guard
case class Impl(g1: Guard, g2: Guard) extends Guard
case class Equiv(g1: Guard, g2: Guard) extends Guard
case object True extends Guard

/// STATEMENTS
//case class SGuard(g: Guard) extends Statement
case class IntAssgn(v: String, d: Int) extends Statement
case class VarAssgn(v1: String, v2: String) extends Statement
case class FunAssgn(v1:String, v2:String, f: UnFunction) extends Statement
case class DataAssgn(v: String, d: Any) extends Statement
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


