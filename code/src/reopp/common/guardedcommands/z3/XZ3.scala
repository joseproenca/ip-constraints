package reopp.common.guardedcommands.z3

import reopp.common.guardedcommands._
import _root_.z3.scala._
import scala.collection.mutable.ArrayBuffer
import reopp.common._

import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import reopp.common.Utils._
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
import reopp.common

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 17/05/13.
 */
object XZ3 {
  /**
   * Solve an integer formula using Z3
   * @param gcs formula to be solved
   * @return Possible solution to the formula
   */
  def solvexz3(gcs: Formula): Option[GCSolution] = {
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    val theory = buildTheory(z3) // each theory has state, including buffering of intermediate values. Cannot be reused.
    val z3ast = gc2xz3(gcs,theory)
//    println("------------------")
//    for (z <- z3ast) println(z)
//    for (z <- theory.extraAssigns) println(z)
//    println("------------------")
    solvexz3(z3ast,theory)
  }

  def solvexz3(const: Iterable[Z3AST],theory: XTheory): Option[GCSolution] = {
    val z3 = theory.getContext
    for (z <- theory.extraAssigns)
      z3.assertCnstr(z)
    for (z <- const)
      z3.assertCnstr(z)

    z3.checkAndGetModel() match {
      case (None, m) =>
//        println("Z3 failed. The reason is: " + z3.getSearchFailure.message)
        None
      case (Some(false), m) =>
//        println("Unsat.")
        None
      case (Some(true), model) => {
        Some(GCWrapper(new XZ3Solution(theory,model),theory.buf))
      }
    }

  }

  private def buildTheory(z3: Z3Context) = new XTheory(z3)

  private class XTheory(z3: Z3Context) extends ProceduralAttachment[Int](z3) {
    // store data: index is the integer passed used as id
    val data = collection.mutable.ArrayBuffer[Any]()
    // store variables for each name (string)
    val vars = collection.mutable.Map[String,Z3AST]()
    // store cached values
    val buf = new Buffer

    // EXTRA TROUBLE to get the model: special variables that simply assigned to a constant number.
    //  - [static] valVars: special variable  -->  index of real data
    //  - [static] fvars:   (function,var)    -->  negative index (to be in the constraints)
    //  - [dynamc] fvals:   negative index    -->  real data
    //  - [dynamc] fvalseed: seed for the negative indexes
    //  - [static] extraAssigns : List[extra z3 constraints]
    // store one time variables to all used constants, to recover the model later
    val valVars = collection.mutable.Map[Z3AST,Int]()
    val fvars   = collection.mutable.Map[AnyRef,Int]()
    val fvals   = collection.mutable.Map[Int,Any]()
    var fvarseed = -1
    val extraAssigns = collection.mutable.ArrayBuffer[Z3AST]()

    // adds the new data value, and returns its new index
    private def xgen(d: Any): Int = {
      data.append(d)
//      println("new data generated["+data.size+"]: "+d)
      data.size
    }

    // same as xgen, but updates fvals by linking the negative index (from fvars) to the new data
    private def xgen(d: Any, ref:AnyRef): Int = {
      if (fvars contains ref) {
        fvals(fvars(ref)) = d
//        println("updating valVars: "+fvars(ref)+" -> "+d)
        fvars(ref)
      }
      else
        xgen(d)
    }

    def getData(index:Int): Option[Any] = {
//      println("searching for data at "+index+" in "+data.mkString("[",",","]")+" or in "+fvals.mkString("[",",","]"))
      val updIndex = index-1
      if (data isDefinedAt updIndex) Some(data(updIndex))
      else fvals.get(index)
    }

    def xpred(p: reopp.common.Predicate, v:String) = {
      extraAssigns += z3.mkImplies(
        z3.mkNot(z3.mkBoolConst(z3.mkStringSymbol(Utils.data2flow(v)))),
        getVar(v) === constant(0))
      predicate( n => {
//        println("testing if "+p+"(#"+n+")")
        getData(n) match {
          case Some(d) => buf.check(p,d) == 1
          case _       => false
        }
      })
    }

    def xfun(f: reopp.common.Function, v: String) = {
      fvars((f,v)) = fvarseed
      val varVal = variable
      valVars(varVal) = fvarseed
//      println("new valVar: "+varVal+" -> "+fvarseed)
      fvarseed -= 1
      extraAssigns += (varVal === constant(fvarseed+1))
      extraAssigns += z3.mkImplies(
        z3.mkNot(z3.mkBoolConst(z3.mkStringSymbol(Utils.data2flow(v)))),
        getVar(v) === constant(0))

      function( n =>
        getData(n) match {
          case Some(d) => xgen(buf.calculate(f,d),(f,v))
          case _       => 0 // return an undefined value in "data"
        }
      )
    }

    def xfun2(f: reopp.common.Function, vs: List[String]) = {
      fvars((f,vs)) = fvarseed
      val varVal = variable
      valVars(varVal) = fvarseed
      //      println("new valVar: "+varVal+" -> "+fvarseed)
      fvarseed -= 1
      extraAssigns += (varVal === constant(fvarseed+1))
      for (v <- vs)
        extraAssigns += z3.mkImplies(
          z3.mkNot(z3.mkBoolConst(z3.mkStringSymbol(Utils.data2flow(v)))),
          getVar(v) === constant(0))


      function( (n1,n2) =>
        (getData(n1),getData(n2)) match {
          case (Some(d1),Some(d2)) => xgen(buf.calculate(f,List(d1,d2)),(f,vs))
          case _ => 0 // undefined value in "daa"
        }
      )
    }
//    def xfun2(f: reopp.common.Function) = function( (n1,n2) =>
//      if ((data isDefinedAt n1) && (data isDefinedAt n2))
//        xgen(buf.calculate(f,List(data(n1),data(n2))))
//      else
//        -1
//    )
//    def xfuns(f: reopp.common.Function) = function( n =>
//      if (data isDefinedAt n) {
//        //xgen(f.calculate(data(n)))
//        xgen(buf.calculate(f,data(n)))
//      }
//      else
//        -1 // return an undefined value in "data"
//    )
    def mkAssign (v: Z3AST, d: Any): Z3AST = {
      val indx = xgen(d)
      val newConstant = constant(indx)
//      println("new data assgn: data["+indx+"]  = "+newConstant+" - sort "+newConstant.getSort)
      val varVal = variable
      valVars(varVal) = indx
//      println("new valVar: "+varVal+" -> "+indx)
      extraAssigns += (varVal === newConstant)
      v === newConstant
    }

//    val isEq      = predicate((s1,s2) => s2 == s1)

    def getVar(s:String): Z3AST = {
      if (vars contains s) vars(s)
      else {
        vars(s) = variable
//        println("new data var: "+s+" -- "+vars(s))
        vars(s)
      }
    }

//    def getVallllll(x: Z3AST,model: Z3Model): Option[Any] = {
//      val v = model.eval(x)
//      // only way I found was by analysing the toString... terrible.
//      if (!v.isDefined) return None
//      val elems = v.get.toString().split("!")
//      if (elems isDefinedAt 2) {
//        // assume: after ! there is always an integer
//        //  with the index of the data -- may be a wrong assumption.
//        val index = Integer.valueOf(elems(2))
//        if (data isDefinedAt index)
//          Some(data(index))
//        else None
//      }
//      else
//        None
//    }
    def getVal(x: Z3AST,model: Z3Model, solValVars: collection.mutable.Map[Z3AST,Any]): Option[Any] = {
      val v = model.eval(x)
//      println("getting value in "+x+" - got "+v)
//      if (v.isDefined) println("solValVars has this?: "+solValVars.keys.mkString(", "))
      if (v.isDefined && solValVars.contains(v.get))
        Some(solValVars(v.get))
      else
        None
    }

  }

  def GCWrapper(s:XZ3Solution,buf: Buffer) = {
    val sol = new GCSolution(s,Map()) {
      override def getDataOn(end:String) = s.getDataOn(end)
    }
    sol.buf = Some(buf)
    sol
  }
  private class XZ3Solution(theory: XTheory, model: Z3Model) extends Solution {
    private val z3 = theory.getContext

    // map values in the solution (e.g., ts--441326531!val!1) to valid data
    private val solValVars =
      for (v <- theory.valVars
           if model.eval(v._1).isDefined && theory.getData(v._2).isDefined)
      yield tmp(v._1) -> theory.getData(v._2).get

    private def tmp(v:Z3AST) = {
//      println("what is the value in "+v+"? - "+model.eval(v).get)
      model.eval(v).get
    }

    def hasFlowOn(end: String) =model.evalAs[Boolean](z3.mkBoolConst(z3.mkStringSymbol(end))) match {
      case None => false
      case Some(b) => b
    }

    def getDataOn(end: String) =
      theory.getVal(theory.getVar(end),model,solValVars)
//      model.evalAs[Int](z3.mkBoolConst(z3.mkStringSymbol(end)))

    override def toString = {
      var res = ""
      for ((v,ast) <- model.getModelConstantInterpretations if v.getRange.isBoolSort)
        res += Utils.ppFlowVar(v.getName.toString()) + " -> " + ast + "\n"
      for ((v,ast) <- theory.vars)
        res += Utils.ppDataVar(v) + " ~> "+theory.getVal(ast,model,solValVars)+"\n"
//      res += "full model\n"
//      res += model.toString()
//      res += "data (init)\n"
//      for (d <- theory.data)
//        res += "[[ "+d+" ]]\n"
//      res += "data variables (init)\n"
//      for (d <- theory.valVars)
//        res += "[[ "+d._1+" -> "+d._2+" ]]\n"
//      res += "fvars\n"
//      for (d <- theory.fvars)
//        res += "[[ "+d._1+" -> "+d._2+" ]]\n"
//      res += "fvals\n"
//      for (d <- theory.fvals)
//        res += "[[ "+d._1+" -> "+d._2+" ]]\n"
//      res += "solValVars\n"
//      for (d <- solValVars)
//        res += "[[ "+d._1+" -> "+d._2+" ]]\n"
      res
    }
  }

  /**
   * Produce a z3 formula for external functions and predicates.
   * @param gcs
   * @param theory
   * @return
   */
  private def gc2xz3(gcs: Formula, theory: XTheory): Iterable[Z3AST] = {
    //    gcs.close() // add flow constraints here
    val z3 = theory.getContext

    var res = new ArrayBuffer[Z3AST]
    for (com <- gcs.commands)
      res += gc2xz3(com,theory)
//      res = z3.mkAnd(res,gc2xz3(com,theory))

    // ADD FLOW CONSRAINTS -- NO NEED: added in Formula.
//    val bvars = new ListBuffer[String]()
//    gcs.bfv(bvars)
////    println("bool vars: "+bvars.mkString("-"))
//    if (!bvars.isEmpty) {
//      val fst = bvars.head
//      var flowConstraint = z3.mkBoolConst(z3.mkStringSymbol(fst))
//      for (v <- bvars.tail)
//        flowConstraint = z3.mkOr(flowConstraint,z3.mkBoolConst(z3.mkStringSymbol(v)))
//      res += flowConstraint
//    }

    // ADD NOFLOW AXIOM -- no need: only added before predicates and functions to avoid incomplete theories
//    for (bv <- bvars)
//      theory.extraAssigns += z3.mkImplies(
//        z3.mkNot(z3.mkBoolConst(z3.mkStringSymbol(bv))),
//        theory.getVar(Utils.flow2data(bv)) === theory.constant(-9999))

    res
  }

  /**
   * Produce a z3 formula from a guarded command
   * @param gc
   * @param theory
   * @return
   */
  private def gc2xz3(gc: GuardedCom, theory: XTheory): Z3AST =
    theory.getContext.mkImplies(gc2xz3(gc.g,theory), gc2xz3(gc.st,theory))


  /**
   *
   * @param g
   * @param theory
   * @return
   */
  private def gc2xz3(g: Guard, theory: XTheory): Z3AST = {
    val z3 = theory.getContext
    g match {
      case Var(name) => z3.mkBoolConst(z3.mkStringSymbol(name))
      // normal z3 integer predicate
      // case IntPred(v, p) => p.z3Pred(z3,z3.mkIntConst(z3.mkStringSymbol(v)))
      case IntPred(v, p) =>  gc2xz3(Pred(v,p),theory)
      // External predicate, managed by the theory
      case Pred(v, p) => theory.xpred(p,v)(theory.getVar(v))
      case And(g1, g2) => z3.mkAnd(gc2xz3(g1,theory),gc2xz3(g2,theory))
      case Or(g1, g2) => z3.mkOr(gc2xz3(g1,theory),gc2xz3(g2,theory))
      case Neg(g) => z3.mkNot(gc2xz3(g,theory))
      case Impl(g1, g2) => z3.mkImplies(gc2xz3(g1,theory),gc2xz3(g2,theory))
      case guardedcommands.Equiv(g1, g2) =>
        z3.mkEq(gc2xz3(g1,theory),gc2xz3(g2,theory))
      case True => z3.mkTrue()
    }
  }

  private def gc2xz3(s: Statement, theory: XTheory): Z3AST = {
    val z3 = theory.getContext
    s match {
      case g: Guard => gc2xz3(g,theory)
      case IntAssgn(v, d) =>
        theory.mkAssign(theory.getVar(v),d)
//        z3.mkEq(z3.mkIntConst(z3.mkStringSymbol(v)),z3.mkInt(d,z3.mkIntSort()))
      case VarAssgn(v1, v2) =>
//        theory.isEq(theory.getVar(v1),theory.getVar(v2))
        z3.mkEq(theory.getVar(v1),theory.getVar(v2))
      case FunAssgn(v1, v2, f: reopp.common.Function) =>
        z3.mkAnd(
          z3.mkEq(theory.getVar(v1),theory.xfun(f,v2)(theory.getVar(v2)))
        )
      case NFunAssgn(v1, v2s, nf: reopp.common.Function) =>
        if (v2s.size == 2)
          z3.mkEq(theory.getVar(v1),theory.xfun2(nf,v2s)(theory.getVar(v2s(0)),theory.getVar(v2s(1))))
        else
          throw new RuntimeException("N-ary data functions for xz3 can only have 2 arguments: "+nf)
      case DataAssgn(v, d) => //throw new RuntimeException("General data assignments not handled with Z3")
          theory.mkAssign(theory.getVar(v),d)
      case Seq(Nil) => z3.mkTrue()
      case Seq(x::xs) => z3.mkAnd(gc2xz3(x,theory),gc2xz3(Seq(xs),theory))
    }
  }


  ////////////////////////////////////////////////////////////////////
  //////////////////////////              ////////////////////////////
  //////////////////////////    TESTS     ////////////////////////////
  //////////////////////////              ////////////////////////////
  ////////////////////////////////////////////////////////////////////


  /**
   * Test the usage of z3
   * @param args
   */
  def main(args:Array[String]) {

    val succ = common.Function("succ") {
      case x:Int =>
        //x+1
        println("succ of "+x+"?")
        readInt()
      case other =>
        println("strange value received: "+other+": "+other.getClass)
    }
    val odd  = common.Predicate("odd") {
      case x:Int => (x % 2) == 1
      case x    => {println("strange value - "+x+": "+x.getClass()); false}
    }

    val subtr = common.Function("subtract"){
        case n:Int => {println("subtracting "+n+" - 5"); n - 5}
//        case List(n:Int) => {println("subtracting "+n); n - 5}
//      case List(n1:Int) => 0 - n1
//      case List(n1:Int,n2:Int) => n1 - n2
//      case List(n1:Int,n2:Int,n3:Int) => (n1 - n2) - n3
          case x => sys.error("wrong arguments for subtract "+x+":"+x.getClass)
    }

    def is(n:Int) = Predicate("(=="+n+")") {
      case x: Int => x == n
    }

//
//    val conn =
//      writer("a1",List(11)) ++
//      writer("a2",List(12)) ++
//      merger("a1","a2","a") ++
//      transf("a","b",subtr) ++
//      sfilter("b","c",odd) ++
//      reader("c",2)
//
//
//    //    conn.run
//    val step = conn.getConstraints.solveXZ3
//    if (step.isDefined) conn.update(step)
//    println("step: "+step)
//
//
//    ///////////////////////
//    ///////////////////////
//
//
    val hackUser = Function("hack") {
      case s:String => {println("hacking "+s); s}
    }
    val undoHack = Function("undo") {
      case s:String => {println("undoing "+s); s}
    }
    val recallUser = Function("recal") {
      case s:String => {println("recalling "+s); s}
    }
    val checkPwd= Predicate("check") {
      case s:String => {println("checking "+s); true}
    }

//    val conn2 = writer("alex",List("alex")) ++
//      writer("bob", List("bob")) ++
//      writer("cindy", List("cindy")) ++
//      merger("bob","cindy","bobcindy") ++
//      transf("alex","a1",hackUser,undoHack) ++
//      transf("a2", "a3",recallUser) ++
//      filter("a1", "a2",checkPwd) ++
//      filter("bobcindy", "b1",checkPwd) ++
//      merger("a3","b1","out") ++
//      reader("out",2) ++
//      // at least one should have flow
//      merger("alex","bobcindy","m") ++
//      sdrain("m","out")

    val conn3 = writer("alex",List("alex")) ++
      writer("bob", List("bob")) ++
      filter("alex","a1",checkPwd)
//      filter("bob", "b1",checkPwd) ++
//      merger("alex","bob","out") ++
//      reader("out",2) ++
//      flow("out")
      // at least one should have flow
//      merger("alex","bob","m") ++
//      sdrain("m","out")

//    conn3.run
    var step3 = conn3.getConstraints.solveXZ3
    println("step3: "+step3)
    if (step3.isDefined) conn3.update(step3)
    step3 = conn3.getConstraints.solveXZ3
    println("step3_b: "+step3)


    val alwaysTrue = Predicate("alwaysTrue") { case _ => true }


    val a = mkVar("a")
    val b = mkVar("b")
    val c = mkVar("c")
    val tmpForm = Formula(
//      a, // redundant if there is only one port
//      !a --> (a := 4),
      !a --> (a :< alwaysTrue)
//          True -->  b /\ (b := 3),
//          a := (subtr,b)
    //      a --> (a := 11),
    //      b --> (b := 12),
    //      c --> (a \/ b),
    //      (a \/ b) --> c,
    //      !(a and b),
    //      a --> (c := a),
    //      b --> (c := b)
    )

    println("PROBLEM: False -> P(^a), where no possible data for ^a exists, cannot be handelled.")
    println("SOLUTION?: extra constraint for each P(^a) [and f(^a)] with (true & ^a := NoData)?")
    println("(still incomplete theory sometimes)")
    println("SOLUTION?: extra constraint for each P(^a) [and f(^a)] with (!a -> ^a := NoData)?")
    println("(seems better. New assumption: !a -> P(a) cannot occur - incomplete theory)")
    println("SOLUTION?: for all ports, add (!a -> ^a := NoData)? - no need yet.")
    println("")
    println("tmpStep: "+tmpForm)
    val tmpStep = tmpForm.solveXZ3
    println("tmpStep: "+tmpStep)


    def ask(i:Int) = Function("ask-" + i) {
      case s:String =>
        println(i + ". "+s)
        readLine()
    }
    val conn4 = writer("a",List("A?")) ++
      transf("a","b",ask(1)) ++
      transf("b","c",ask(2)) ++
      transf("c","d",ask(3)) ++
      reader("d")
    conn4.run()
  }
}


