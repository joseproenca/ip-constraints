package reopp.common.guardedcommands.z3

import _root_.z3.scala._
import _root_.z3.scala.dsl.Distinct
import _root_.z3.scala.dsl.IntVar
import _root_.z3.scala.dsl.{Distinct, IntVar}
import reopp.common.guardedcommands._
import reopp.common.guardedcommands.Var
import reopp.common._
import Utils._
import collection.mutable.{Map => MutMap, ListBuffer}
import reopp.common.examples.Even
import reopp.common
import reopp.common.guardedcommands.IntPred
import reopp.common.guardedcommands.GuardedCom
import reopp.common.guardedcommands.FunAssgn
import reopp.common.guardedcommands.VarAssgn
import scala.Some
import reopp.common.guardedcommands.Pred
import reopp.common.guardedcommands.And
import reopp.common.guardedcommands.Seq
import reopp.common.guardedcommands.Or
import reopp.common.guardedcommands.Neg
import reopp.common.guardedcommands.Var
import reopp.common.guardedcommands.DataAssgn
import reopp.common.guardedcommands.Impl
import reopp.common.guardedcommands.NFunAssgn
import reopp.common.guardedcommands.IntAssgn


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 31/07/12
 * Time: 12:30
 * To change this template use File | Settings | File Templates.
 */

object Z3 {


  /**
   * Solve an integer formula using Z3
   * @param gcs formula to be solved
   * @return Possible solution to the formula
   */
  def solvez3(gcs: Formula): OptionSol[Z3Solution] = {
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    val z3ast = gc2z3(gcs,z3)
    solvez3(z3ast,z3)
  }

  /**
   * Solve a data formula using Z3 and predicate abstraction for SAT.
   * @param gcs formula to be solved
   * @return possible solution to the formula
   */
  def solveboolz3(gcs: Formula): OptionSol[Z3Solution] = {
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    val z3term = gc2boolz3(gcs,z3)
    Z3.solvez3(z3term,z3)
  }


  /**
   * Solve a given z3 term for a given z3 context
   * @param const term to be solved
   * @param z3 context
   * @return possible solution to const
   */
  def solvez3(const: Z3AST, z3: Z3Context): OptionSol[Z3Solution] = {
    z3.assertCnstr(const)

    z3.checkAndGetModel() match {
      case (None, m) =>
        println("Z3 failed. The reason is: " + z3.getSearchFailure.message)
        NoneSol()
      case (Some(false), m) =>
        println("Unsat.")
        NoneSol()
      case (Some(true), model) => {
        //        for (c <- model.getModelConstants) println ("const "+c)
        //        println("v1: " + model.evalAs[Boolean](v1))
        //        println("v2: " + model.evalAs[Boolean](v2))
        //        println("v3: " + model.evalAs[Boolean](v3))
//        println("model:\n"+model)
        SomeSol(new Z3Solution(z3,model))
        //        model.delete
      }
    }
  }

  def gc2z3(gcs: Formula, z3: Z3Context): Z3AST = {
//    gcs.close() // add flow constraints here

    var res = z3.mkTrue()
    for (com <- gcs.commands)
      res = z3.mkAnd(res,gc2z3(com,z3))

    // ADD FLOW CONSRAINTS
    val bvars = new ListBuffer[String]()
    gcs.bfv(bvars)
//    println("bool vars: "+bvars.mkString("-"))
    if (!bvars.isEmpty) {
      val fst = bvars.head
      var flowConstraint = z3.mkBoolConst(z3.mkStringSymbol(fst))
      for (v <- bvars.tail)
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
    case FunAssgn(v1, v2, f:IntFunction) =>
      z3.mkEq(z3.mkIntConst(z3.mkStringSymbol(v1)),
        f.z3Fun(z3,List(z3.mkIntConst(z3.mkStringSymbol(v2)))))
    case NFunAssgn(v1, v2s, f:IntFunction) =>
      z3.mkEq(z3.mkIntConst(z3.mkStringSymbol(v1)),
        f.z3Fun(z3, v2s.map((v2:String) => z3.mkIntConst(z3.mkStringSymbol(v2)))))
    case FunAssgn(v1, v2, notIntF) =>
      throw new RuntimeException("General data functions have no associated z3 functions: "+notIntF)
    case NFunAssgn(v1, v2s, notIntF) =>
      throw new RuntimeException("General data functions have no associated z3 functions: "+notIntF)
    case DataAssgn(v, d) => //throw new RuntimeException("General data assignments not handled with Z3")
      if (d.isInstanceOf[Int])
        z3.mkEq(z3.mkIntConst(z3.mkStringSymbol(v)),z3.mkInt(d.asInstanceOf[Int],z3.mkIntSort()))
      else
        throw new RuntimeException("General data assignments not handled with Z3")
    case Seq(Nil) => z3.mkTrue()
    case Seq(x::xs) => z3.mkAnd(gc2z3(x,z3),gc2z3(Seq(xs),z3))
  }


//  def solveBool(c: CNF.Core, vars: MutMap[String,Int], z3: Z3Context): Option[GCBoolSolution] = {
//
//  }


  /**
   * Converts a guarded command into a Z3 term.
   * @param gcs the guarded command to be converted
   * @param z3 the Z3 context, used to build operators and variables
   * @return a Z3 term equivalent to the predicate abstraction of the given guarded command
   */
  def gc2boolz3(gcs: Formula,z3: Z3Context): Z3AST = {
    val da = gcs.getDA
    var res = z3.mkTrue()
    for (com <- gcs.commands)
      res = z3.mkAnd(res,gc2boolz3(com,da,z3))

    // ADD FLOW CONSRAINTS
    val bvars = new ListBuffer[String]()
    gcs.bfv(bvars)
    if (!bvars.isEmpty) {
      val fst = bvars.head
      var flowConstraint = z3.mkBoolConst(z3.mkStringSymbol(fst))
      for (v <- bvars.tail)
        flowConstraint = z3.mkOr(flowConstraint,z3.mkBoolConst(z3.mkStringSymbol(v)))
      res = z3.mkAnd(res,flowConstraint)
    }

    res
  }

  def gc2boolz3(gc: GuardedCom, da: DomainAbst, z3: Z3Context): Z3AST =
    z3.mkImplies(gc2boolz3(gc.g,z3), gc2boolz3(gc.st,da,z3))

  def gc2boolz3(g: Guard, z3: Z3Context): Z3AST = g match {
    case Var(name)     => z3.mkBoolConst(z3.mkStringSymbol(name))
    case IntPred(v, p) => z3.mkBoolConst(z3.mkStringSymbol(mkPredVar(v,p,List())))
    case Pred(v, p)    => z3.mkBoolConst(z3.mkStringSymbol(mkPredVar(v,p,List())))
    case And(g1, g2)   => z3.mkAnd(gc2boolz3(g1,z3),gc2boolz3(g2,z3))
    case Or(g1, g2)    => z3.mkOr(gc2boolz3(g1,z3),gc2boolz3(g2,z3))
    case Neg(g)        => z3.mkNot(gc2boolz3(g,z3))
    case Impl(g1, g2)  => z3.mkImplies(gc2boolz3(g1,z3),gc2boolz3(g2,z3))
    case guardedcommands.Equiv(g1, g2) => z3.mkEq(gc2boolz3(g1,z3),gc2boolz3(g2,z3))
    case True          => z3.mkTrue()
  }


  def gc2boolz3(s: Statement, da: DomainAbst, z3: Z3Context): Z3AST = s match {
    case g: Guard => gc2boolz3(g,z3)
    case IntAssgn(v, d) => gc2boolz3(DataAssgn(v,Int.box(d)),da,z3)
    case DataAssgn(v, d) =>
      var res: Z3AST = z3.mkTrue()
      val dom = da.domain(v)
      for ((pred,fs) <- dom) {
        var newd: Any = d
        for (f<-fs.reverse) newd = f.calculate(newd)
//        println("Adding data assignm.: "+predVar(v,pred,fs)+" to true/false")
        if  (pred.check(newd))
          res = z3.mkAnd(res,z3.mkBoolConst(z3.mkStringSymbol(mkPredVar(v,pred,fs))))
        else
          res = z3.mkAnd(res,z3.mkNot(z3.mkBoolConst(z3.mkStringSymbol(mkPredVar(v,pred,fs)))))
      }
      res
    case VarAssgn(v1, v2) =>
      val (d1,d2) = (da.domain(v1), da.domain(v2))
      var res: Z3AST = z3.mkTrue()
      for((pred,fs) <- d1)
        if (d2 contains (pred,fs)) {
//          println("Adding pred equiv: "+predVar(v1,pred,fs)+"<->"+predVar(v2,pred,fs))
          val t = gc2boolz3(Var(mkPredVar(v1,pred,fs)) <-> Var(mkPredVar(v2,pred,fs)),da,z3)
          res = z3.mkAnd(res,t)
        }
      res
    case FunAssgn(v1, v2, f) =>
      val (d1,d2) = (da.domain(v1), da.domain(v2))
      var res: Z3AST = z3.mkTrue()
      for((pred,fs) <- d1)
        if (d2 contains (pred,f::fs)) {
//          println("Adding func assignm.: "+predVar(v1,pred,fs)+" <- "+predVar(v2,pred,f::fs))
          val t = gc2boolz3(Var(mkPredVar(v1,pred,fs)) <-> Var(mkPredVar(v2,pred,f::fs)),da,z3)
          res = z3.mkAnd(res,t)
        }
      res
    case NFunAssgn(v,v2s,f) => v2s match {
      case List(v2) => gc2boolz3(FunAssgn(v,v2,f),da,z3)
      case Nil => throw new Exception("Predicate abstraction cannot be applied to n-ary functions - "+f)
    }
    case Seq(Nil) => z3.mkTrue()
    case Seq(x::xs) => z3.mkAnd(gc2boolz3(x,da,z3),gc2boolz3(Seq(xs),da,z3))
  }

  /*
    case g: Guard => g.toBoolConstrBuilder
    case IntAssgn(v, d) => DataAssgn(v,d).toBoolConstrBuilder(da)
    case DataAssgn(v, d) => //reopp.common.choco.DataAssgn(v,d)
      // INSTEAD OF CALCULATING, CREATE A LAZY CONSTRAINT!
      var res:ConstrBuilder = TrueC
      for ((pred,fs,xflow) <- da.domainWithEnd(v)) {

//        var newd = d
//        for (f<-fs.reverse) newd = f.calculate(newd)
//        if (pred.check(newd))
//          res = res and reopp.common.choco.Var(predVar(v,pred,fs))
//        else
//          res = res and reopp.common.choco.Neg(reopp.common.choco.Var(predVar(v,pred,fs)))

        println("added LazyPred("+predVar(v,pred,fs)+","+data2flow(v)+","+data2flow(xflow)+","+fs+")")
        res = res and LazyPred(predVar(v,pred,fs),data2flow(v),data2flow(xflow),d,pred,fs)
      }
      res

    case VarAssgn(v1, v2) =>
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: ConstrBuilder= TrueC
      for ((pred,fs) <- d2)
        if (d1 contains (pred,fs)) {
          val t = reopp.common.choco.VarEq(predVar(v1,pred,fs),predVar(v2,pred,fs))
          res = res and t
        }
      res

    case FunAssgn(v1,v2,f) =>
//      VarAssgn(v1,v2).toBoolConstrBuilder(da)
      val (d1,d2) = (da.domain(v1),da.domain(v2))
      var res: ConstrBuilder= TrueC
      for ((pred,fs) <- d1)
        if (d2 contains (pred,f::fs)) {
          val t = reopp.common.choco.VarEq(predVar(v1,pred,fs),predVar(v2,pred,f::fs))
          res = res and t
        }
      res

    case Seq(Nil) => reopp.common.choco.TrueC
    case Seq(s::ss) => s.toBoolConstrBuilder(da) and Seq(ss).toBoolConstrBuilder(da)

////////////////////////////

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

   */


  /**
   * Test the usage of z3
   * @param args
   */
  def main(args:Array[String]) {

    println("------------ testing procedural attachments ---")

    val z33 = new Z3Context()
    // Defines a new theory of strings with two predicates and one function symbol.
    val strings = new ProceduralAttachment[String](z33) {
      val oddLength = predicate(s ⇒ s.length % 2 == 1)
      val isSubstr = predicate((s1,s2) ⇒ s2.contains(s1))
      val concat = function((s1,s2) ⇒ s1 + s2)
    }

    import strings._
    val s1, s2 = strings.variable
    z33.assertCnstr(
      z33.mkAnd(
        z33.mkAnd( s1 === constant("hello"),
          z33.mkOr(s2 === constant("world"),s2 === constant("moon"))
        ),
        z33.mkAnd(oddLength(concat(s2, s1)), isSubstr(constant("low"), concat(s1,s2)))
      )
    )

//      s1 === "hello") && ((s2 === "world") || (s2 === "moon")) &&
//          oddLength(concat(s2, s1)) && isSubstr("low", concat(s1,s2)))
    println(z33.check) // unsatisfiable


    println("------------ testing my procedural attachments for real ---")
    val z = new Z3Context()
    val xint = new ProceduralAttachment[Int](z) {
      // index: ith element added --> value added.
      val data = collection.mutable.ArrayBuffer[Any]()
      def xpred(p: common.Predicate) = predicate( n => {
        println("testing if "+p+"(#"+n+")")
        if (data isDefinedAt n) {
          //          println("from "+data(n.toString)+" got "+p.check(data(n.toString)))
          p.check(data(n))
        }
        else false
      })
      def xfun(f: common.Function) = function( n =>
        if (data isDefinedAt n) {
          xgen(f.calculate(data(n)))
        }
        else
          -1 // return an undefined value in "data"
      )
      // adds the new data value, and returns its new index
      def xgen(d: Any): Int = { data.append(d); data.size - 1 }
      def mkEq (v: Z3AST, d: Any): Z3AST = {
        val indx = xgen(d)
        val newConstant = constant(indx)
        println("new val: data["+(indx)+"]  = "+newConstant+" - sort "+newConstant.getSort)
        v === newConstant
      }

      def getVal(x: Z3AST,model: Z3Model): Option[Any] = {
        val v = model.eval(x)
        // only way I found was by analysing the toString... terrible.
        if (!v.isDefined) return None
        val elems = v.get.toString().split("!")
        if (elems isDefinedAt 2) {
          // assume: after ! there is always an integer
          val index = Integer.valueOf(elems(2))
          if (data isDefinedAt index)
            Some(data(index))
          else None
        }
        else
          None
      }
    }

    val succ = common.Function("succ") {
      case x:Int =>
        //x+1
        println("succ of "+x+"?")
        readInt()
    }
    val odd  = common.Predicate("odd") {
      case x:Int => (x % 2) == 1
      case x    => {println("strange value - "+x+": "+x.getClass()); false}
    }


    import xint.{mkEq,xpred,xfun}
    val a,b,c,d,e = xint.variable
    // if no "mkEq", no generator of elements of the theory exists - theory incomplete.
    // if one "mkEq" is left out, the solver experiments all generated elements of the theory.
    z.assertCnstr(mkEq(a,6))
    z.assertCnstr(mkEq(b,9))
    z.assertCnstr(z.mkEq(c,b))
    z.assertCnstr(z.mkEq(d,a))
    z.assertCnstr(z.mkEq(e,xfun(succ)(d)))
    z.assertCnstr(xpred(odd)(e)) // odd(c) true, odd(d) false, odd(e) true.

    z.checkAndGetModel() match {
      case (None, _) => println("Z3 failed. The reason is: " + z.getSearchFailure.message)
      case (Some(false), _) => println("Unsat.")
      case (Some(true), model) => {
        println("solution: "+model)
        println("a: "+xint.getVal(a,model))
        println("b: "+xint.getVal(b,model))
        println("c: "+xint.getVal(c,model))
        println("d: "+xint.getVal(d,model))
        println("e: "+xint.getVal(e,model))
      }
    }






    println("------------ testing my own procedural attachments ---")
    val myz3 = new Z3Context()

    ///////////////////////////
    // Defines a new theory of integers (my constants) with external predicates and functions
    val external = new ProceduralAttachment[Int](myz3) {
      val data = collection.mutable.Map[String,Any]()
      var seed = 0
      var testConst: Z3AST = null
      def xpred(p: common.Predicate) = predicate( n => {
//        println("testing if "+p+"(#"+n+")")
        if (data contains n.toString) {
//          println("from "+data(n.toString)+" got "+p.check(data(n.toString)))
          p.check(data(n.toString))
        }
        else false
        // default - false
      })

      def xfun(f: common.Function) = function( n =>
        if (data contains n.toString) {
          xgen(f.calculate(data(n.toString)))
//          seed = seed + 1
//          data(seed.toString) = f.calculate(data(n.toString))
//          seed
        }
        else
          0
      )
      def xgen(d: Any): Int = { seed += 1 ; data(seed.toString) = d; seed }
      def xassign(v: Z3AST, d: Any): Z3AST = {
        xgen(d)
//        println("asserting "+v+" === "+seed)
//        v === "a"
//        v.===(myz3.mkConst(seed.toString,v.getSort))
        val newconstant = constant(seed)
        testConst = newconstant
        println("new val: "+seed+" - "+newconstant+" - sort "+newconstant.getSort)
        v === newconstant
      }
    }
    ///////////////////////////

    // SMTLib -

    import external._
    val x,y,v,w = external.variable
//    val xx = myz3.mkConst(myz3.mkStringSymbol("xx"),myz3.sort)
    println("var x/y/w/z: "+x+" / "+y+" / "+w+" / "+z+" / ")
    myz3.assertCnstr(myz3.mkAnd(xassign(x,5), external.xpred(odd)(y)))
    myz3.assertCnstr(myz3.mkEq(x,y))
    myz3.assertCnstr(xassign(w,6))
    myz3.assertCnstr(xassign(v,7))
//    val a = xgen(5)
//    myz3.assertCnstr(x === a)
    myz3.checkAndGetModel() match {
      case (None, _) => println("Z3 failed. The reason is: " + myz3.getSearchFailure.message)
      case (Some(false), _) => println("Unsat.")
      case (Some(true), model) => {
        println("model:\n"+model)
        val valOnX = model.eval(x)
        println("value on x: "+
//          model.eval(x) + " - "+
//          _root_.z3.scala.Z3
          valOnX +" : "+valOnX.get.getClass +
//          external.
//          model.evalAs[Int](x) +
          "\n - equals to generated constant? "+(valOnX.get == external.testConst)

        ) //(myz3.mkBoolConst(myz3.mkStringSymbol("x"))
        for (e <- external.getElems) println(" - elem "+e+": "+e.getClass+" - "+e.getSort)
      }
    }

    // seems that the context myz3 has a solver, and has in the same line as the var the "constant added".
    println("PRINTING context: "+myz3)

    /////////////////
    // NEW THEORY: the toString of the model.eval(x) ends with "val!0". This could
    // mean that it is the first value of this sort. I generate them, so I can keep track of them.
    // SEEMS OK!!
    /////////////////

    println(external.data)



    println("------------ solving simple constraint (3 vars) ---")

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

    println("------------ solving queens problem - 8x8 - 92 solutions")

    val zz3 = new Z3Context("MODEL" -> true)
    val N = 8
//    val cols = (0 until N) map { _ => IntVar() } // column vars
    val cols = (0 until N) map { i => IntVar() } // IntVar() } // column vars
    val diﬀCnstr = Distinct(cols : _*) // all queens on distinct cols
    val boundsCnstr = for (c <- cols) yield (c >= 0 && c < N) // cols are within bounds
    val diagonalsCnstr = // no two queens on same diagonal
      for (i <- 0 until N; j <- 0 until i) yield
        ((cols(i) - cols(j) !== i - j) && (cols(i) - cols(j) !== j - i))
    zz3.assertCnstr(diﬀCnstr)
    boundsCnstr map (zz3.assertCnstr(_))
    diagonalsCnstr map (zz3.assertCnstr(_))
    println(zz3.checkAndGetAllModels.size) // prints 92


    zz3.checkAndGetModel() match {
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
