package common.beh.benchmarks

import _root_.z3.scala.{Z3AST, Z3Config, Z3Context}
import common.beh.guardedcommands._
import common.beh.Utils._
import common.beh.guardedcommands.Var
import common.beh.guardedcommands.dataconnectors.ConstraintGen._
import scala.Some
import common.beh.{Solution, IntPredicate}
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import z3.Z3

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 19/09/12
 * Time: 16:06
 * To change this template use File | Settings | File Templates.
 */
class AllSchedules

object AllSchedules extends App {

  /// PARSE ARGUMENTS ///
//  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
  else               200
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

  ///////////////////
  /// The PROBLEM ///
  ///////////////////

  val morning = new Morning
  val evening  = new Evening

  def genSched(i:Int,on: Boolean): GuardedCommands = {

    //    new GCWriter("x",i,List(500)).constraints ++
    val res =
      exrouter("x","a","b",i) ++
      negfilter("a","e",evening,i) ++
      filter("a","f",evening,i) ++
      filter("b","g",morning,i) ++
      merger("e","g","m",i) ++
      sdrain("a","c",i) ++
      sdrain("b","d",i) ++
      sdrain("g","b",i) ++
      sync("e","disp",i) ++
      sync("f","off",i) ++
      sync("g","on",i) ++
      GuardedCommands(Var(flowVar("x",i)))

    if (on) res ++
      sfifo("m","c",Some(0),i) ++
      fifo("f","d",None,i)
    else res ++
      sfifo("m","c",None,i) ++
      fifo("f","d",Some(0),i)
  }


  def genScheds(uids: Iterable[Int], startVar: String, on: Boolean): GuardedCommands = {
    var res = new GuardedCommands()
    for (i <- uids) {
      res ++= genSched(i,on)
      // manual replicator from (startVar.startUid) to (x,i)
      val av = Var(flowVar(startVar,0))
      val bv = Var(flowVar("x",i))
      res ++= GuardedCommands(
        av <-> bv,
        av --> (bv := av) //VarAssgn(dataVar("x",i), dataVar(startVar,startUid))
      )
    }
    res
  }



  //  val problem = genSched(0,true)  ++ new GCWriter("x",0,List(500)).constraints ++ // on, morning  - display
  //                GuardedCommands(True --> SGuard(Var(flowVar("e",0))))

  //  val schedule = genSched(0,true)  ++ new GCWriter("x",0,List(1400)).constraints // on, evening  - turn off
  //  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(500)).constraints  // off, morning - turn on
  //  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(1400)).constraints // off, evening - no sol

  val n2 = n / 2

  val problem = genScheds(1 to n2, "time",true) ++   // some will display
    genScheds(n2+1 to n, "time",false) ++            // and some will turn on
    writer("time",List(500)) ++                        // (it is morning)
    GuardedCommands(True --> Var(flowVar("time",0)))   // require some dataflow



  /////////////////////////
  /// Running the tests ///
  /////////////////////////

  if (justInit) problem.justInit = true


  if (satfull) {
    val time = System.currentTimeMillis()
    val res = problem.solve
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
//    if (res.isDefined) println("quick-z3  - solved in "+spent+" ms:\n"+res.get.pretty)
//    else println("quick-z3  - no solution (in "+spent+" ms)")
    println("quick-z3  - "+spent)

      //// SAT-FULL ////
      time = System.currentTimeMillis()
      res = problem.solve
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
//    if (res.isDefined) println("Z3 - solved in "+spent+" ms:\n"+res.get.pretty)
//    else println("Z3 - no solution (in "+spent+" ms)")
    println("Z3        - "+spent)

    //// LAZY-SAT ////
    time = System.currentTimeMillis()
    res = problem.lazyDataSolve
    spent = System.currentTimeMillis() - time
    //    if (res.isDefined) println("lazy-sat - solved in "+spent+" ms:\n"+res.get.pretty)
    //    else println("lazy-sat - no solution (in "+spent+" ms)")
    println("lazy-sat  - "+spent)


  }

  class Morning extends IntPredicate {
    // from 7am to 10am
    val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,420),Choco.leq(x,600))
    val funPred = (x:Int) => (x >= 40) && (x <= 600)
    val z3Pred = (z:Z3Context,v:Z3AST) => //z.mkGT(v,z.mkInt(i,z.mkIntSort()))
      z.mkAnd(
        z.mkGE(v,z.mkInt(420,z.mkIntSort())),
        z.mkLE(v,z.mkInt(600,z.mkIntSort())))
    override def toString = "Morning"
  }
  class Evening extends IntPredicate {
    // from 7pm to midnight
    val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,1140),Choco.leq(x,1440))
    val funPred = (x:Int) => (x >= 1140) && (x <= 1440)
    val z3Pred = (z:Z3Context,v:Z3AST) => //z.mkGT(v,z.mkInt(i,z.mkIntSort()))
      z.mkAnd(
        z.mkGE(v,z.mkInt(1140,z.mkIntSort())),
        z.mkLE(v,z.mkInt(1440,z.mkIntSort())))
    override def toString = "Evening"
  }
}
