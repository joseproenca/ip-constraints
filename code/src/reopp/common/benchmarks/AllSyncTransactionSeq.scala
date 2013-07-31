package reopp.common.benchmarks

import reopp.common.guardedcommands.Formula
import reopp.common.guardedcommands.dataconnectors.ConstraintGen._
import reopp.common.benchmarks.AllSyncTransaction.genTransaction
import z3.scala.{Z3Config, Z3Context}
import reopp.common.guardedcommands.z3.Z3
import reopp.common.Solution

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 21/09/12
 * Time: 14:11
 * To change this template use File | Settings | File Templates.
 */

class AllSyncTransactionSeq
object AllSyncTransactionSeq extends App {

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

  val invFunc = new InvFunc

  def genTransations(max:Int,failAt:Int): Formula = {
    var res = genTransaction(1,invFunc)
    for (seed <- 2 to max) {
      if (seed == failAt) res ++= genTransaction(-1 * seed,invFunc)
      else                res ++= genTransaction(seed,invFunc)
    }
    res
  }


  ///
  // Sequential
  def problem =
  //    genTransations(n,((n-1)/2)+2) ++
  //    genTransations(n,2) ++
    genTransations(n,n) ++
    writer("x1",List(2)) ++
    writer("y"+(n+1),List()) ++
    sync("x2","RESULT") ++
    sync("y1","ABORTED")



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

    //// DYN-CHOCO ////
    time = System.currentTimeMillis()
    res = problem.solveChocoDyn
    spent = System.currentTimeMillis() - time
    //    if (res.isDefined) println("lazy-sat - solved in "+spent+" ms:\n"+res.get.pretty)
    //    else println("lazy-sat - no solution (in "+spent+" ms)")
    println("dyn-choco  - "+spent)


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