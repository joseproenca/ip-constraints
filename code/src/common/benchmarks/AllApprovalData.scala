package common.benchmarks

import _root_.z3.scala.{Z3Config, Z3AST, Z3Context}
import common.guardedcommands._
import common.guardedcommands.dataconnectors._
import scala.math.pow
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import common.{Predicate, Solution, IntPredicate, Utils}
import Utils._
import common.guardedcommands.IntPred
import z3.Z3
import common.guardedcommands.dataconnectors.ConstraintGen._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 21/06/12
 * Time: 10:25
 * To change this template use File | Settings | File Templates.
 */

class AllApprovalData

object AllApprovalData extends App {

  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
  else               8
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



  def genClients(n:Int): Iterable[GCWriter] = {
    var res = List[GCWriter]()
    for (i <- n to 1 by -1) {
      res ::=
        new GCWriter("w"+i,0,List((i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5,(i*6 % 16)+5))) // tuple
//        new GCWriter("w"+i,0,List(join(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5)))
//      println("new writer: "+(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5)+ " -- "+
//        join(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5))
    }
    res
  }

  def genMergers(height:Int): Formula= {
    val size = pow(2,height)
    var srcs = List("x")
    var res = Formula()
    for (level <- 1 to height) {
      var newsrcs = List[String]()
      for (x <- srcs) {
        res ++= merger(x+"1",x+"2",x)
        newsrcs :::= List(x+"1",x+"2")
      }
      srcs = newsrcs
    }
//    println("size / n.of srcs: "+size+"/"+srcs.size)
//    println("clients: "+genClients(size.toInt).map(_.x))
    for (wr <- genClients(size.toInt)) {
      srcs match {
        case hd::tl =>
          res ++= (wr.getConstraints ++ sync(wr.x,hd))
          srcs = tl
        case Nil => {}
      }
    }

    res
  }


  val approve = Predicate("Approve") {
    case x:(Int,Int,Int,Int,Int) =>  (x._2*2 + x._3*2 + x._4*3 + x._5*5) >= 140
    case x => throw new RuntimeException("unexpeced type "+x)
  }

  val deny = Predicate("Deny") {
    case x:(Int,Int,Int,Int,Int) =>  (x._2*2 + x._3*2 + x._4*3 + x._5*5) <= 90
    case x => throw new RuntimeException("unexpeced type "+x)
  }


  val problem = genMergers(n) ++
    filter("x","app-ok",approve) ++
    filter("x","den-ok",deny) ++
    filter("x","neither-ok", Neg(Pred(dataVar("x",0),approve)) and
                             Neg(Pred(dataVar("x",0),deny)))
//    flow("x") ++
//    flow("app-ok")
//    writer("x",List(19))

  if (justInit) problem.justInit = true


  else if (quicksat) {
    val time = System.currentTimeMillis()
    val res = problem.quickDataSolve
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
  else if (lazyy) {
    val time = System.currentTimeMillis()
    val res = problem.lazyDataSolve
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (satfull || chocosat || choco || z3)
    print(0)
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

    // THESE ARE NOT IMPLEMENTED: only for
//    //// SAT-FULL ////
//    time = System.currentTimeMillis()
//    res = problem.solveIterative
//    spent = System.currentTimeMillis() - time
////    if (res.isDefined) println("SAT-full - solved in "+spent+" ms:\n"+res.get.pretty)
////    else println("SAT-full - no solution (in "+spent+" ms)")
//    println("SAT-full  - "+spent)
//
//    //// SATC-FULL ////
//    time = System.currentTimeMillis()
//    res = problem.solveChocoSat
//    spent = System.currentTimeMillis() - time
////    if (res.isDefined) println("SATC-full - solved in "+spent+" ms:\n"+res.get.pretty)
////    else println("SATC-full - no solution (in "+spent+" ms)")
//    println("SATC-full - "+spent)
//
//    //// CHOCO ////
//    time = System.currentTimeMillis()
//    res = problem.solveChoco
//    spent = System.currentTimeMillis() - time
////    if (res.isDefined) println("Choco - solved in "+spent+" ms:\n"+res.get.pretty)
////    else println("Choco - no solution (in "+spent+" ms)")
//    println("Choco     - "+spent)
//
//    /// Z3 ////
//    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
//    time = System.currentTimeMillis()
//    res = Z3.solvez3(Z3.gc2z3(problem,z3),z3)
//    spent = System.currentTimeMillis() - time
////    if (res.isDefined) println("Z3 - solved in "+spent+" ms:\n"+res.get.pretty)
////    else println("Z3 - no solution (in "+spent+" ms)")
//    println("Z3        - "+spent)

    //// QUICK-SAT-Z3 ////
    val zz3 = new Z3Context(new Z3Config("MODEL" -> true))
    time = System.currentTimeMillis()
    res = problem.quickDataSolve(zz3)
    spent = System.currentTimeMillis() - time
//    if (res.isDefined) println("quick-z3  - solved in "+spent+" ms:\n"+res.get.pretty)
//    else println("quick-z3  - no solution (in "+spent+" ms)")
    if (res.isDefined) println("ok/accept/neither: "+
      res.get.getDataOn(dataVar("app-ok",0))+"/"+
      res.get.getDataOn(dataVar("den-ok",0))+"/"+
      res.get.getDataOn(dataVar("neither-ok",0))+"/"
    )
    println("quick-z3  - "+spent)

      // LAZY-SAT ////
    time = System.currentTimeMillis()
    res = problem.lazyDataSolve
    spent = System.currentTimeMillis() - time
//    if (res.isDefined) println("lazy-sat - solved in "+spent+" ms:\n"+res)
//    else println("lazy-sat - no solution (in "+spent+" ms)")
    if (res.isDefined) println("ok/accept/neither: "+
      res.get.getDataOn(dataVar("app-ok",0))+"/"+
      res.get.getDataOn(dataVar("den-ok",0))+"/"+
      res.get.getDataOn(dataVar("neither-ok",0))+"/"
    )
    println("lazy-sat  - "+spent)

    // ChocoDyn ////
    time = System.currentTimeMillis()
    res = problem.solveChocoDyn
    spent = System.currentTimeMillis() - time
//        if (res.isDefined) println("lazy-sat - solved in "+spent+" ms:\n"+res)
//        else println("lazy-sat - no solution (in "+spent+" ms)")
    if (res.isDefined) println("ok/accept/neither: "+
      res.get.getDataOn(dataVar("app-ok",0))+"/"+
      res.get.getDataOn(dataVar("den-ok",0))+"/"+
      res.get.getDataOn(dataVar("neither-ok",0))+"/"
    )
    println("choco dyn tables  - "+spent)

    // ChocoDyn ////
    time = System.currentTimeMillis()
    res = problem.solveXZ3
    spent = System.currentTimeMillis() - time
//        if (res.isDefined) println("lazy-sat - solved in "+spent+" ms:\n"+res)
//        else println("lazy-sat - no solution (in "+spent+" ms)")
    if (res.isDefined) println("ok/accept/neither: "+
      res.get.getDataOn(dataVar("app-ok",0))+"/"+
      res.get.getDataOn(dataVar("den-ok",0))+"/"+
      res.get.getDataOn(dataVar("neither-ok",0))+"/"
      )
    println("X-Z3 - "+spent)
  }

}


