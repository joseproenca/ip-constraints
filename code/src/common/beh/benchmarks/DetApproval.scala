package common.beh.benchmarks

import _root_.z3.scala.{Z3Config, Z3AST, Z3Context}
import common.beh.guardedcommands.dataconnectors.GCMerger
import common.beh.guardedcommands.dataconnectors.GCSync
import common.beh.guardedcommands.dataconnectors.GCWriter
import common.beh.guardedcommands.{Pred, GuardedCommands, Neg, IntPred}
import scala.math.pow
import common.beh.{Predicate, Solution, IntPredicate}
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import common.beh.Utils._
import common.beh.guardedcommands.dataconnectors.ConstraintGen._
import common.beh.guardedcommands.z3.Z3
import common.beh.choco.genericconstraints.{UnFunction, UnPredicate}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 01/08/12
 * Time: 15:27
 * To change this template use File | Settings | File Templates.
 */

class DetApproval

object DetApproval extends App {

//  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
  else               1
  val quicksat = if (args.size > 1) args(1) startsWith "q"
  else               false
  val lazyy = if (args.size > 1) args(1) startsWith "l"
  else               false



  def genMergers(height:Int): GuardedCommands= {
//    val size = pow(2,height)
    var srcs = List("x")
    var res = GuardedCommands()
    for (level <- 1 to height) {
      var newsrcs = List[String]()
      for (x <- srcs) {
        res = res ++ new GCMerger(x+1,x+2,x,0).constraints
        newsrcs :::= List(x+1,x+2)
      }
      srcs = newsrcs
    }
    var i = 0
    for (src <- srcs) {
      res ++= writer(src,List((i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5)))
      i += 1
    }

    res
  }

  val approve = new UnPredicate {
    def check(x: Any) = x match {
      case v: (Int,Int,Int,Int) => (v._2*2 + v._3*3 + v._4*5) >= 140
      case _ =>throw new RuntimeException("checking an unknown type")
    }
    override def toString = "Approve"
  }

  val deny = new UnPredicate() {
    def check(x: Any) = x match {
      case v: (Int,Int,Int,Int) => (v._2*2 + v._3*3 + v._4*5) <= 90
      case _ => throw new RuntimeException("checking an unknown type")
    }
    override def toString = "Deny"
  }

//  val confirm = new UnFunction() {
//    def calculate(x: Any) = x match {
//      case v: (Int,Int,Int,Int) =>
//        println("Output writer "+v._1+" with grades "+v._2+"/"+v._3+"/"+v._4)
//        v
//    }
//    override def toString = "Confirm"
//  }


  val problem = genMergers(n) ++
    filter("x","app-ok",approve) ++
    filter("x","den-ok",deny) ++
    filter("x","neither-ok", Neg(Pred(dataVar("x",0),approve)) and
      Neg(Pred(dataVar("x",0),deny))) //++
//    transf("x","y",confirm) ++ filter("y","z",approve)

  //    flow("x") ++
  //    flow("app-ok")
  //    writer("x",List(19))


  if (quicksat) {
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

    //// QUICK-SAT ////
    var time = System.currentTimeMillis()
    var res: Option[Solution] = problem.quickDataSolve
    var spent = System.currentTimeMillis() - time
    if (res.isDefined) println("quick-sat - solved in "+spent+" ms:\n"+res.get.pretty)
    else println("quick-sat - no solution (in "+spent+" ms)")
//    println("quick-sat - "+spent)

    //// LAZY-SAT ////
    time = System.currentTimeMillis()
    res = problem.lazyDataSolve
    spent = System.currentTimeMillis() - time
    if (res.isDefined) println("lazy-sat - solved in "+spent+" ms:\n"+res.get.pretty)
    else println("lazy-sat - no solution (in "+spent+" ms)")
//    println("lazy-sat  - "+spent)
  }

}