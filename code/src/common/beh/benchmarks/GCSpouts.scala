package common.beh.benchmarks

import common.beh.Utils._
import common.beh.guardedcommands.connectors._
import common.beh.choco.dataconnectors.{LT, GT, Predicate}

//import common.beh.guardedcommands.GuardedCommands
import common.beh.guardedcommands._
import choco.kernel.model.variables.integer.IntegerVariable
import choco.Choco

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 12/06/12
 * Time: 13:42
 * To change this template use File | Settings | File Templates.
 */

class GCSpouts

object GCSpouts extends App {

  val lfive = new LT(5)
  val gtwo= new GT(2)

//  def genSched(i:Int,on: Boolean): GuardedCommands = {
//
//    //    new GCWriter("x",i,List(500)).constraints ++
//    val res =
//      new GCExRouter("x","a","b",i).constraints ++
//        new GCFilter("a","e",i,Neg(Pred(evening,dataVar("a",i)))).constraints ++
//        new GCFilter("a","f",i,Pred(evening,dataVar("a",i))).constraints ++
//        new GCFilter("b","g",i,Pred(morning,dataVar("b",i))).constraints ++
//        new GCMerger("e","g","m",i).constraints ++
//        new GCSDrain("a","c",i).constraints ++
//        new GCSDrain("b","d",i).constraints ++
//        new GCSDrain("g","b",i).constraints ++
//        new GCSync("e","disp",i).constraints ++
//        new GCSync("f","off",i).constraints ++
//        new GCSync("g","on",i).constraints ++
//        GuardedCommands(True --> SGuard(Var(flowVar("x",i))))
//
//    if (on) res ++
//      new GCSyncFifo("m","c",Some(0),i).constraints ++
//      new GCFifo("f","d",None,i).constraints
//    else res ++
//      new GCSyncFifo("m","c",None,i).constraints ++
//      new GCFifo("f","d",Some(0),i).constraints
//  }


  def genFilters(times: Int): GuardedCommands = {
    var res = new GuardedCommands()
    for (i <- 0 to (times-1)) {
      res ++= new GCFilter("a","b",i,Pred(gtwo,dataVar("a",i))).constraints ++
              new GCFilter("b","c",i,Neg(Pred(lfive,dataVar("b",i)))).constraints
      // manual replicator from (startVar.startUid) to (x,i)
    }
    res ++= new GCReader("c",times-1,1).constraints
    res
  }

  def genSpout(): GuardedCommands = {
    new GCSpout("x","a",0).constraints ++
      new GCFilter("x","reader",0,(Pred(lfive,dataVar("x",0)))).constraints ++
      new GCReader("reader",0,1).constraints
  }



  val problem = genFilters(1)  ++ genSpout() ++
//    GuardedCommands(True --> SGuard(Var(flowVar("reader",0)))) ++
    GuardedCommands(True --> VarAssgn(dataVar("x",0),dataVar("a",0)))

  //  val schedule = genSched(0,true)  ++ new GCWriter("x",0,List(1400)).constraints // on, evening  - turn off
  //  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(500)).constraints  // off, morning - turn on
  //  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(1400)).constraints // off, evening - no sol

  //  val problem = genScheds(100 to 200, "time",0,true) ++ // some will display
  //    genScheds(500 to 600, "time",0,false) ++            // and some will turn on
  //    new GCWriter("time",0,List(500)).constraints        // (it is morning)



  val time = System.currentTimeMillis()
  //  val time = System.nanoTime()
  val cnf = problem.toCNF
  val res = problem.solveBool(cnf._1,cnf._2)
  val spent = System.currentTimeMillis() - time

  if (res.isDefined) println("solved in "+spent+" ms.")
  else println("no solution (in "+spent+" ms)")

  if (res.isDefined) println(res.get.pretty)
  if (res.isDefined) {
    val pEval = problem.partialEval(res.get)
    println("partial eval: "+pEval)
    pEval.quotient()
    println("quotient:     "+pEval)
    pEval.applyDataAssgn(res.get)
    println("dataAssign:   "+pEval)
    pEval.solveSimpleData(res.get,problem.da)
    println("solveData:    "+pEval)
    //    val mapRest    = problem.applyDataAssgn(res.get)
//    val newMapRest = problem.solveSimpleData(res.get,mapRest._1,mapRest._2)
//    val newSol     = problem.incrementAndSolve(cnf._1,cnf._2,res.get,newMapRest._1,newMapRest._2)
//
//    println("assignVars: "+mapRest)
//    println("solveSimpleData: "+newMapRest)
//    if (newSol.isDefined) println("incrementalSol:\n"+newSol.get.pretty)
  }



  class Morning extends Predicate {
    // from 7am to 10am
    val choPred = (x:IntegerVariable) => Choco.and(Choco.geq(x,420),Choco.leq(x,600))
    val funPred = (x:Int) => (x >= 40) && (x <= 600)
    override def toString = "Morning"
  }
  class Evening extends Predicate {
    // from 7pm to midnight
    val choPred = (x:IntegerVariable) => Choco.and(Choco.geq(x,1140),Choco.leq(x,1440))
    val funPred = (x:Int) => (x >= 1140) && (x <= 1440)
    override def toString = "Evening"
  }
}
