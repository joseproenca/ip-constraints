package reopp.common.guardedcommands

import reopp.common
import collection.mutable.{Set => MutSet, Map => MutMap, ListBuffer}
import collection.immutable.{Set => ImSet}
import reopp.common._
import choco.LazyPred
import Utils._
import reopp.common.choco.{LazyPred, TrueC, ConstrBuilder}
import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:25
 * To change the template use File | Settings | File Templates.
 */
case class GuardedCom(g:Guard, st: Statement) {

  def optimiseEqVars(vars: mutable.Map[String, String]) {
    if (g == True)
      GuardedCom(True,st.optimiseEqVars(vars))
    else {
      for (v <- fv) vars(v) = v
      this
    }
  }


  def fv:Set[String] = g.fv ++ st.fv
  def fv2(s:MutSet[String]) {g.fv2(s); st.fv2(s)}
  def bfv(l:ListBuffer[String]) = {g.bfv(l); st.bfv(l)}
  def solveDomain(da:DomainAbst)  { g.solveDomain(da); st.solveDomain(da) }

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
  def afv2(dom: DomainAbst,vars: MutMap[String,Int]) {
    g.afv2(dom,vars)
    st.afv2(dom,vars)
  }



  def toCNF(vars: MutMap[String,Int],da: DomainAbst): CNF.Core =  {
    // recalculate "not g or st"
    var list: List[Array[Int]] = List()
    for (cl1 <- Neg(g).toCNF(vars,da); cl2 <- st.toCNF(vars,da))
      list = (cl1 ++ cl2) :: list
    list
  }
  def toCNF2(vars: MutMap[String,Int],da: DomainAbst, list: CNF2.Core) =  {
    // recalculate "not g or st"
    if (g != True) {
      val l1: ListBuffer[Array[Int]] = ListBuffer()
      val l2: ListBuffer[Array[Int]] = ListBuffer()
      Neg(g).toCNF2(vars,da,l1)
      st.toCNF2(vars,da,l2)
      for (cl1 <- l1; cl2 <- l2)
        list += cl1 ++ cl2
    }
    else
      st.toCNF2(vars,da,list)
  }


  def partialEval(sol:Solution): PEval = { //(Map[String,Int], Map[String,String]) = {
    if (g.eval(sol)) {
//      println("  # PE ("+g.toString+"): "+st.toString)
      st.partialEval(sol)
    }
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
  def /\(e: Guard) = And(this,e)
  def or(e: Guard) = Or(this,e)
  def \/(e: Guard) = Or(this,e)
  def ->(e: Guard) = Impl(this,e)
  def -->(e: Statement) = GuardedCom(this,e)
  def <->(e: Guard) = Equiv(this,e)
  def unary_! = Neg(this)

  override def optimiseEqVars(vars: mutable.Map[String, String]): Guard =
    // TODO: under construction
    throw new RuntimeException("Under construction")


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
  }

  override def fv2(s: MutSet[String]): Unit = this match {
    case Var(name) => s += name
    case IntPred(v, p) => s += v
    case Pred(v, p) => s += v
    case And(g1, g2) => {g1.fv2(s); g2.fv2(s)}
    case Or(g1, g2) => {g1.fv2(s); g2.fv2(s)}
    case Neg(g) => g.fv2(s)
    case Impl(g1, g2) => {g1.fv2(s); g2.fv2(s)}
    case Equiv(g1, g2) => {g1.fv2(s); g2.fv2(s)}
    case True => {}
  }

  override def afv2(da:DomainAbst,vs:MutMap[String,Int]): Unit = this match {
    case Var(name) => updD(da,vs,name)
    case IntPred(v, p) => for ((p,fs) <- da.domain(v)) updD(da,vs,predVar(v,p,fs))
    case Pred(v, p) => for ((p,fs) <- da.domain(v)) updD(da,vs,predVar(v,p,fs))
    case And(g1, g2) => {g1.afv2(da,vs); g2.afv2(da,vs)}
    case Or(g1, g2) => {g1.afv2(da,vs); g2.afv2(da,vs)}
    case Neg(g) => g.afv2(da,vs)
    case Impl(g1, g2) => {g1.afv2(da,vs); g2.afv2(da,vs)}
    case Equiv(g1, g2) => {g1.afv2(da,vs); g2.afv2(da,vs)}
    case True => {}
  }

  override def bfv(l:ListBuffer[String]): Unit = this match {
    case Var(name) => if (!l.contains(name)) l += name
    case And(g1, g2) => {g1.bfv(l); g2.bfv(l)}
    case Or(g1, g2) => {g1.bfv(l); g2.bfv(l)}
    case Neg(g) => g.bfv(l)
    case Impl(g1, g2) => {g1.bfv(l); g2.bfv(l)}
    case Equiv(g1, g2) => {g1.bfv(l); g2.bfv(l)}
    case _ => {}
  }

  override def solveDomain(da: DomainAbst): Unit = this match {
    case Var(name) =>  {}
    case IntPred(v, p) => da += (v,p)
    case Pred(v, p) => da += (v,p)
    case And(g1, g2) => {g1.solveDomain(da); g2.solveDomain(da)}
    case Or(g1, g2) => {g1.solveDomain(da); g2.solveDomain(da)}
    case Neg(g) => g.solveDomain(da)
    case Impl(g1, g2) => {g1.solveDomain(da); g2.solveDomain(da)}
    case Equiv(g1, g2) => {g1.solveDomain(da); g2.solveDomain(da)}
    case True => {}
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
  }
  override def toCNF2(vars: MutMap[String,Int],da: DomainAbst,l:CNF2.Core): Unit = this match {
    case Impl(e1,e2) => (Neg(e1) or e2).toCNF2(vars,da,l)
    case Neg(Impl(e1,e2)) => (Neg(e2) and e1).toCNF2(vars,da,l)
    case Equiv(e1,e2) => ((e1 -> e2) and (e2 -> e1)).toCNF2(vars,da,l)
    case Neg(Equiv(e1,e2)) => (Neg(e1 -> e2) or Neg(e2 -> e1)).toCNF2(vars,da,l)
    case And(c1,c2) => {c1.toCNF2(vars,da,l); c2.toCNF2(vars,da,l)}
    case Neg(And(e1,e2)) => (Neg(e1) or Neg(e2)).toCNF2(vars,da,l)
    case Or(c1,c2) => {
      val l1: ListBuffer[Array[Int]] = ListBuffer()
      val l2: ListBuffer[Array[Int]] = ListBuffer()
      c1.toCNF2(vars,da,l1)
      c2.toCNF2(vars,da,l2)
      for (cl1 <- l1; cl2 <- l2)
        l += cl1 ++ cl2
    }
    case Neg(Or(e1,e2)) => (Neg(e1) and Neg(e2)).toCNF2(vars,da,l)
    case Var(a) => l += Array(vars(a))
    case Neg(Var(a)) => l += Array(vars(a)*(-1))
    case IntPred(v, p) => Var(predVar(v,p,List())).toCNF2(vars,da,l)
    //      if (da.domain(v) contains p) Var(predVar(v,p)).toCNF(vars,da)
    //      else List(Array())
    case Neg(IntPred(v, p)) => l += Array(vars(predVar(v,p,List()))*(-1))
    //      Neg(Var(predVar(v,p))).toCNF(vars,da)
    case Pred(v, p) => Var(predVar(v,p,List())).toCNF2(vars,da,l)
    case Neg(Pred(v, p)) => l += Array(vars(predVar(v,p,List()))*(-1))
    case True => {}
    case Neg(True) => l += Array()
    case Neg(Neg(a)) => a.toCNF2(vars,da,l)
  }

  private def join(c1: CNF2.Core, c2:CNF2.Core) = {
    c1 ++= c2
    c1
  }


  def eval(sol: Solution): Boolean = this match {
    case Var(name) => sol.hasFlowOn(name)
    case IntPred(v, p) => sol.hasFlowOn(predVar(v,p,List()))
    case Pred(v, p) => sol.hasFlowOn(predVar(v,p,List()))
    case And(g1, g2) => g1.eval(sol) && g2.eval(sol)
    case Or(g1, g2) => g1.eval(sol) || g2.eval(sol)
    case Neg(g) => !g.eval(sol)
    case Impl(g1, g2) => !g1.eval(sol) || g2.eval(sol)
    case Equiv(g1, g2) => ((g1->g2) and (g2->g1)).eval(sol)
    case True => true
  }

  override def toString = this match {
    case Var(name) => ppFlowVar(name)
    case IntPred(v, p) => p.toString+"("+ppDataVar(v)+")"
    case Pred(v, p) => p.toString+"("+ppDataVar(v)+")"
    case And(g1, g2) => g1.mbPar + " /\\ " + g2.mbPar
    case Or(g1, g2) => g1.mbPar + " \\/ " + g2.mbPar
    case Neg(g) => "¬"+g.mbPar
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
  def /\(s: Statement) = and(s)

  def and(s: Statement) = this match {
    case Seq(s1) => s match {
      case Seq(s2) => Seq(s1:::s2)
      case y => Seq(s1:::List(y))
    }
    case x => s match {
      case Seq(s2) => Seq(x::s2)
      case y => Seq(List(this,s))
    }
  }

  def optimiseEqVars(vars: mutable.Map[String, String]): Statement =
    throw new RuntimeException("under construction.")
//  this match {
//    case g: Guard => g.optimiseEqVars(vars)
//    case IntAssgn(v, d) => { vars(v) = v; this }
//    case VarAssgn(v1, v2) =>
//      if (vars contains v1) vars(.......)
//    case FunAssgn(v1, v2, f) =>
//    case DataAssgn(v, d) =>
//    case Seq(sts) =>
//  }


  /** Collects all (free) variables. Functional implementation. */
  def fv: Set[String] = this match {
    case g: Guard => g.fv
    case IntAssgn(v, _) => Set(v)
    case DataAssgn(v, _) => Set(v)
    case VarAssgn(v1, v2) => Set(v1,v2)
    case FunAssgn(v1,v2,_) => Set(v1,v2)
    case NFunAssgn(v1,vs,_) => vs.toSet + v1
    case Seq(Nil) => Set()
    case Seq(s::ss) => s.fv ++ Seq(ss).fv
//    case g: Guard => super.fv
  }
  /** Collects all (free) variables. Uses an accumulator for efficiency. */
  def fv2(s: MutSet[String]): Unit = this match {
    case g: Guard => g.fv2(s)
    case IntAssgn(v, _) => s += v
    case DataAssgn(v, _) => s += v
    case VarAssgn(v1, v2) => {s += v1; s += v2}
    case FunAssgn(v1,v2,_) => {s += v1; s += v2}
    case NFunAssgn(v1,vs,_) => {s += v1; s ++= vs}
    case Seq(Nil) => {}
    case Seq(st::sts) => {st.fv2(s); Seq(sts).fv2(s)}
  }
  def afv2(da:DomainAbst,vs:MutMap[String,Int]): Unit = this match {
    case g: Guard => g.afv2(da,vs)
    case IntAssgn(v, _)  => for ((p,fs) <- da.domain(v)) updD(da,vs,predVar(v,p,fs))
    case DataAssgn(v, _) => for ((p,fs) <- da.domain(v)) updD(da,vs,predVar(v,p,fs))
    case VarAssgn(v1, v2) =>
      for ((p,fs) <- da.domain(v1)) updD(da,vs,predVar(v1,p,fs))
      for ((p,fs) <- da.domain(v2)) updD(da,vs,predVar(v2,p,fs))
    case FunAssgn(v1,v2,_) =>
      for ((p,fs) <- da.domain(v1)) updD(da,vs,predVar(v1,p,fs))
      for ((p,fs) <- da.domain(v2)) updD(da,vs,predVar(v2,p,fs))
    case NFunAssgn(v,v2s,f) => v2s match {
      case List(v2) => FunAssgn(v,v2,f).afv2(da,vs)
      case _ => throw new Exception("Predicate abstraction cannot be applied to n-ary functions - "+f)
    }
    case Seq(Nil) => {}
    case Seq(s::ss) => {s.afv2(da,vs); Seq(ss).afv2(da,vs)}
    //    case g: Guard => super.fv
  }
  protected def updD(da:DomainAbst,vars:MutMap[String,Int],v:String) {
    if (!vars.contains(v)) {
      val i = vars("")
      vars(v) = i
      vars("") = i+1
    }
  }

  def bfv(l:ListBuffer[String]): Unit = this match {
    case g: Guard => g.bfv(l)
    case Seq(Nil) =>
    case Seq(s::ss) => {s.bfv(l); Seq(ss).bfv(l)}
    case _ =>
  }

  def solveDomain(da: DomainAbst): Unit = this match {
    case g: Guard => g.solveDomain(da)
    case IntAssgn(_,_) => {}
    case DataAssgn(_,_) => {}
    case VarAssgn(v1, v2) => da += (v2 -> v1)
    case Seq(Nil) => {}
    case Seq(s::ss) => {s.solveDomain(da); Seq(ss).solveDomain(da)}
    case FunAssgn(v1,v2,f) =>
      da += (v2 -> v1)
      da += (v1, f)
    case NFunAssgn(v,vs,f) => vs match {
      case List(v2) => FunAssgn(v,v2,f).solveDomain(da)
      case _ => throw new Exception("Predicate abstraction cannot be applied to n-ary functions - "+f)
    }
//    case g: Guard => super.da
  }

  def toCNF(vars: MutMap[String,Int],da: DomainAbst): CNF.Core = this match {
    case g: Guard => g.toCNF(vars,da)
    case IntAssgn(v,d)  => DataAssgn(v,Int.box(d)).toCNF(vars,da)
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

    case NFunAssgn(v,vs,f) => vs match {
      case List(v2) => FunAssgn(v,v2,f).toCNF(vars,da)
      case _ => throw new Exception("Predicate abstraction cannot be applied to n-ary functions - "+f)
    }
    case Seq(Nil) => List()
    case Seq(s::ss) => s.toCNF(vars,da) ++ Seq(ss).toCNF(vars,da)
  }

  def toCNF2(vars: MutMap[String,Int],da: DomainAbst, l:CNF2.Core ): Unit = this match {
    case g: Guard => g.toCNF2(vars,da,l)
    case IntAssgn(v,d)  => DataAssgn(v,Int.box(d)).toCNF2(vars,da,l)
    case DataAssgn(v,d) =>
      val dom = da.domain(v)
      for ((pred,fs) <- dom) {
        // ALL PRECALCULATED (only in choco-Bool it is lazily calculated)
        var newd: Any = d
        for (f<-fs.reverse) newd = f.calculate(newd)
        if (pred.check(newd)) {
          l += Array(vars(predVar(v,pred,fs)))
        }
        else
          l += Array(vars(predVar(v,pred,fs)) * (-1))
      }
    case VarAssgn(v1, v2) =>
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      for ((pred,fs) <- d1)
        if (d2 contains (pred,fs)) {
          (Var(predVar(v1,pred,fs)) <-> Var(predVar(v2,pred,fs))).toCNF2(vars,da,l)
        }
    case FunAssgn(v1, v2, f) =>
      //      println("converting var eq of "+v1+" and "+v2)
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      for ((pred,fs) <- d1)
        if (d2 contains (pred,f::fs)) {
          (Var(predVar(v1,pred,fs)) <-> Var(predVar(v2,pred,f::fs))).toCNF2(vars,da,l)
        }
    case NFunAssgn(v,vs,f) => vs match {
      case List(v2) => FunAssgn(v,v2,f).toCNF2(vars,da,l)
      case _ => throw new Exception("Predicate abstraction cannot be applied to n-ary functions - "+f)
    }
    case Seq(Nil) => {}
    case Seq(s::ss) => {s.toCNF2(vars,da,l); Seq(ss).toCNF2(vars,da,l) }
  }


  def partialEval(sol: Solution): PEval = this match {
    case g: Guard => new PEval(Map(),Map(),Map())
    case IntAssgn(v, d) =>
      if (sol.hasFlowOn(flowVar(v))) new PEval(Map(v -> d),Map(),Map())
      else new PEval(Map(),Map(),Map())
    case DataAssgn(v, d) =>
      if (sol.hasFlowOn(flowVar(v))) new PEval(Map(v -> d),Map(),Map())
      else new PEval(Map(),Map(),Map())
    case VarAssgn(v1, v2) =>
      if (sol.hasFlowOn(flowVar(v2))) new PEval(Map(),Map(v2 -> ImSet(v1)),Map())
      else new PEval(Map(),Map(),Map())
    // TODO: CHANGE PEval to split at x=f(y), and include this info in PEval
    case FunAssgn(v1, v2, f) =>
      if (sol.hasFlowOn(flowVar(v2))) new PEval(Map(),Map(),Map(v2 -> ImSet((v1,f))))
      else new PEval(Map(),Map(),Map())
    case NFunAssgn(v,vs,f) => vs match {
      case List(v2) => FunAssgn(v,v2,f).partialEval(sol)
      case _ => throw new Exception("Predicate abstraction cannot be applied to n-ary functions - "+f)
    }
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
    case NFunAssgn(v1,v2s,f) => ppDataVar(v1) + " := " + f + v2s.map(ppDataVar(_)).mkString("(",",",")")
    case Seq(Nil) => ""
    case Seq(x::Nil) => x.toString
    case Seq(x::xs) => x + " ; " + xs
//    case g: Guard => super.toString
  }

}

/// concrete GUARDS
case class Var(name: String) extends Guard {
  /** Assignment of data variables. */
  def :=(v:Var): Statement = VarAssgn(dataVar(name),dataVar(v.name))
  /** Assignment of data values. */
  def :== (d:Any): Statement = DataAssgn(dataVar(name),d)
//  def :=(d:Int): Statement = DataAssgn(flow2data(name),Int.box(d))
//  def :=(d: Any): Statement = d match {
//    case (v:Var) => VarAssgn(flow2data(name),flow2data(v.name))
//    case _ => DataAssgn(flow2data(name),d)
//  }
  /** Application of a function to a var, and assignment of the result. */
  def :=(f:common.Function,v:Var): Statement = FunAssgn(dataVar(name),dataVar(v.name),f)
  /** Application of a function to a list of vars, and assignment of the result. */
  def :=(f:common.Function,vs:List[Var]): Statement = NFunAssgn(dataVar(name),vs.map(v => dataVar(v.name)),f)
  /** Guard to check whether the variables belongs to the given predicate. */
  def :< (p:Predicate): Guard = Pred(dataVar(name),p)
  def data = dataVar(name)
  def flow = flowVar(name)
}
case class IntPred(v:String, p: IntPredicate) extends Guard
case class Pred(v:String, p:Predicate) extends Guard
case class And(g1: Guard, g2: Guard) extends Guard
case class Or(g1: Guard, g2: Guard) extends Guard
case class Neg(g1: Guard) extends Guard
case class Impl(g1: Guard, g2: Guard) extends Guard
case class Equiv(g1: Guard, g2: Guard) extends Guard
case object True extends Guard

/// concrete STATEMENTS
//case class SGuard(g: Guard) extends Statement
case class IntAssgn(v: String, d: Int) extends Statement
case class VarAssgn(v1: String, v2: String) extends Statement
case class FunAssgn(v1:String, v2:String, f: common.Function) extends Statement
case class NFunAssgn(v1:String,vs:List[String], f: common.Function) extends Statement
case class DataAssgn(v: String, d: Any) extends Statement
case class Seq(sts: List[Statement]) extends Statement


// CNF
object CNF {
  type Core = List[Array[Int]]
}


// More efficient implementation with listbuffers, used as intermediate structure
object CNF2 {
  type Core = ListBuffer[Array[Int]]
}



