package reopp.common.benchmarks

import _root_.choco.kernel.model.variables.integer.IntegerExpressionVariable
import _root_.choco.Choco
import z3.scala.{Z3Config, Z3AST, Z3Context}
import reopp.common.guardedcommands.Formula
import reopp.common.guardedcommands.z3.Z3
import reopp.common
import reopp.common.guardedcommands.dataconnectors.ConstraintGen._
import reopp.common.{Solution, IntFunction, Predicate, IntPredicate}
import reopp.common.guardedcommands.dataconnectors.ConstraintGen

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 19/09/12
 * Time: 10:30
 * To change this template use File | Settings | File Templates.
 */


class AllSyncTransaction

object AllSyncTransaction extends App {

//  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
  else               40 // 40 - 220
  val satfull = if (args.size > 1) args(1) startsWith "s"
  else               false
  val chocosat = if (args.size > 1) args(1) startsWith "cs"
  else               false
  val choco = if (args.size > 1) (args(1) startsWith "c") && !chocosat
  else               false
  val z3sat = if (args.size > 1) args(1) startsWith "zs"
  else               false
  val z3 = if (args.size > 1) (args(1) startsWith "z") && !z3sat
  else               false
  val quicksat = if (args.size > 1) args(1) startsWith "q"
  else               false
  val lazyy = if (args.size > 1) args(1) startsWith "l"
  else               false
  val justInit = if (args.size > 2) args(2) startsWith "i"
  else               false


  //////////////////////////
  /// Define the problem ///
  //////////////////////////

  def genTransaction(pre: Predicate, post: Predicate, f: common.Function, finv: common.Function,
                      in: String, out: String, aborted: String, abort: String, uid: String): Formula = {
    filter(in,"a"+uid,pre) ++
    transf("a"+uid,"b"+uid,f) ++
    filter("b"+uid,out,post) ++
    negfilter(in,"c"+uid,pre) ++
    negfilter("b"+uid,"d"+uid,post) ++
    transf("e"+uid,"f"+uid,finv) ++
    merger("d"+uid,abort,"e"+uid) ++
    merger("c"+uid,"f"+uid,aborted)
  }

  def genTransaction(seed:Int,invf: common.Function): Formula =
    genTransaction(new PreCond(seed),new PostCond(seed),new SomeFunc(seed),invf,
                  "x"+math.abs(seed),"x"+(math.abs(seed)+1),
                  "y"+math.abs(seed),"y"+(math.abs(seed)+1),math.abs(seed).toString)



  ////// REMOVE!! /////
  def genTransations(max:Int,failAt:Int): Formula = {
    var res = genTransaction(1,invFunc)
    for (seed <- 2 to max) {
      if (seed == failAt) res ++= genTransaction(-1 * seed,invFunc)
      else                res ++= genTransaction(seed,invFunc)
    }
    res
  }


  // Sequential
  def problems =
//    genTransations(n,((n-1)/2)+2) ++
//    genTransations(n,2) ++
    genTransations(n,n) ++
    writer("x1",List(2)) ++
    writer("y"+(n+1),List()) ++
    sync("x2","RESULT") ++
    sync("y1","ABORTED")

  //////// UNTIL HERE....//////////////

  val invFunc = new InvFunc

  ////////////////////////
  def genParTransations(in:String,max:Int): Formula = {
    var res = genTransaction(1,invFunc) ++
      sync(in,"x1") ++
//    println("empty writer for y2")
      writer("y2",List())

    for (seed <- 3 to (max*2) by 2) {
      res ++= genTransaction(seed,invFunc)
      res ++= sync(in,"x"+seed)
      res ++= writer("y"+(seed+1),List())
    }
    res
  }

  // parallel transactions
  def problem =
    genParTransations("start",n) ++
    writer("start",List(2))


  /////////////////////////
  /// Running the tests ///
  /////////////////////////

  if (justInit) problem.justInit = true


  if (satfull) {
    val time = System.currentTimeMillis()
    val res = problem.solveIterative
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (chocosat) {
    val time = System.currentTimeMillis()
    val res = problem.solveChocoSat
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (choco) {
    val time = System.currentTimeMillis()
    val res = problem.solveChoco
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (z3sat) {
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    val time = System.currentTimeMillis()
    val res = problem.quickDataSolve(z3)
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (z3) {
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    val time = System.currentTimeMillis()
    val res = Z3.solvez3(Z3.gc2z3(problem,z3),z3)
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (quicksat) {
    val time = System.currentTimeMillis()
    val res = problem.quickDataSolve
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (lazyy) {
    val time = System.currentTimeMillis()
    val res = problem.lazyDataSolve
    val spent = System.currentTimeMillis() - time
    print(spent)
  }

  /// EXPERIMENTS:
  else {

//    println("  # THE PROBLEM:\n"+problem.commands.mkString(" - ","\n - ","\n"))

    var time: Long = 0
    var res: Option[Solution] = null
    var spent: Long = 0


    //// QUICK-SAT ////
    time = System.currentTimeMillis()
    res = problem.quickDataSolve
    spent = System.currentTimeMillis() - time
//    if (res.isDefined) println("quick-sat - solved in "+spent+" ms:\n"+res.get.pretty)
//    else println("quick-sat - no solution (in "+spent+" ms)")
        println("quick-sat - "+spent)

    //// QUICK-SAT-Z3 ////
    val zz3 = new Z3Context(new Z3Config("MODEL" -> true))
    time = System.currentTimeMillis()
    res = problem.quickDataSolve(zz3)
    spent = System.currentTimeMillis() - time
//        if (res.isDefined) println("quick-z3  - solved in "+spent+" ms:\n"+res.get.pretty)
//        else println("quick-z3  - no solution (in "+spent+" ms)")
    println("quick-z3  - "+spent)

    //// SAT-FULL ////
    time = System.currentTimeMillis()
    res = problem.solveIterative
    spent = System.currentTimeMillis() - time
    //    if (res.isDefined) println("SAT-full - solved in "+spent+" ms:\n"+res.get.pretty)
    //    else println("SAT-full - no solution (in "+spent+" ms)")
    println("SAT-full  - "+spent)

    //// SATC-FULL ////
    time = System.currentTimeMillis()
    res = problem.solveChocoSat
    spent = System.currentTimeMillis() - time
    //    if (res.isDefined) println("SATC-full - solved in "+spent+" ms:\n"+res.get.pretty)
    //    else println("SATC-full - no solution (in "+spent+" ms)")
    println("SATC-full - "+spent)

    //// CHOCO ////
    time = System.currentTimeMillis()
    res = problem.solveChoco
    spent = System.currentTimeMillis() - time
    //    if (res.isDefined) println("Choco - solved in "+spent+" ms:\n"+res.get.pretty)
    //    else println("Choco - no solution (in "+spent+" ms)")
    println("Choco     - "+spent)

    /// Z3 ////
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    time = System.currentTimeMillis()
    res = Z3.solvez3(Z3.gc2z3(problem,z3),z3)
    spent = System.currentTimeMillis() - time
//        if (res.isDefined) println("Z3 - solved in "+spent+" ms:\n"+res.get.pretty)
//        else println("Z3 - no solution (in "+spent+" ms)")
    println("Z3        - "+spent)

    //// LAZY-SAT ////
    time = System.currentTimeMillis()
    res = problem.lazyDataSolve
    spent = System.currentTimeMillis() - time
    //    if (res.isDefined) println("lazy-sat - solved in "+spent+" ms:\n"+res.get.pretty)
    //    else println("lazy-sat - no solution (in "+spent+" ms)")
    println("lazy-sat  - "+spent)


  }
}



/// PREDICATES

class PreCond(seed: Int) extends IntPredicate {
  val funPred = (x:Int) => if (seed<0) false else x*x + seed*seed <= 100
  val z3Pred = (z:Z3Context,v:Z3AST) => if (seed<0) z.mkFalse() else
    z.mkLE(
      z.mkAdd(
        z.mkMul(v,v),
        z.mkMul(z.mkInt(seed,z.mkIntSort()),z.mkInt(seed,z.mkIntSort()))),
      z.mkInt(100,z.mkIntSort()))
  val choPred = (x:IntegerExpressionVariable) => Choco.TRUE

  override def toString = "PreCond("+seed+")"
}

class PostCond(seed: Int) extends IntPredicate {
  val funPred = (x:Int) => if (seed<0) false else x <= seed
  val z3Pred = (z:Z3Context,v:Z3AST) => if (seed<0) z.mkFalse() else
    z.mkLE(
      v,
      z.mkInt(seed,z.mkIntSort()))
  val choPred = (x:IntegerExpressionVariable) => Choco.TRUE
    //throw new RuntimeException("choco pred not defined")

  override def toString = "PostCond("+seed+")"
}

class SomeFunc(seed: Int) extends IntFunction {
  val funFun = (x:Int) => (x*45 + 7) % seed
  val z3Fun = (z:Z3Context,v:List[Z3AST]) =>
    z.mkMod(
      z.mkAdd(
        z.mkMul(
          v.head,
          z.mkInt(45,z.mkIntSort())),
        z.mkInt(7,z.mkIntSort())),
      z.mkInt(seed,z.mkIntSort()))
  val choFun = (x:IntegerExpressionVariable) => x
  // throw new RuntimeException("choco pred not defined")

  override def toString = "[(x*45+7)%"+seed+"]"
}

class InvFunc extends IntFunction {
  val funFun = (x:Int) => x+1000
  val z3Fun = (z:Z3Context,v:List[Z3AST]) =>
    z.mkAdd(
      v.head,
      z.mkInt(1000,z.mkIntSort()))
  val choFun = (x:IntegerExpressionVariable) => x
  // throw new RuntimeException("choco pred not defined")

  override def toString = "[x+1000]"
}
