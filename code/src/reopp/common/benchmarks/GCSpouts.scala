package reopp.common.benchmarks

import reopp.common.{IntPredicate, Utils}
import Utils._
import reopp.common.guardedcommands.dataconnectors._
import reopp.common.examples.{LT, GT}

//import reopp.common.guardedcommands.Formula
import reopp.common.guardedcommands._
import choco.kernel.model.variables.integer.IntegerExpressionVariable
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



  def genFilters(times: Int): Formula = {
    var res = Formula()
    for (i <- 0 to (times-1)) {
      res ++= new GCFilter("a","b",i,IntPred(dataVar("a",i),gtwo)).getConstraints ++
              new GCFilter("b","c",i,Neg(IntPred(dataVar("b",i),lfive))).getConstraints
      // manual replicator from (startVar.startUid) to (x,i)
    }
    res ++= new GCReader("c",times-1,1).getConstraints
    res
  }

  def genSpout(): Formula = {
    new GCSSpout("x","a",0).getConstraints ++
      new GCFilter("x","reader",0,(IntPred(dataVar("x",0),lfive))).getConstraints ++
      new GCReader("reader",0,1).getConstraints
  }



  val problem = genFilters(1)  ++ genSpout() ++
//    Formula(True --> SGuard(Var(flowVar("reader",0)))) ++
    Formula(True --> VarAssgn(dataVar("x",0),dataVar("a",0)))

  //  val schedule = genSched(0,true)  ++ new GCWriter("x",0,List(1400)).getConstraints // on, evening  - turn off
  //  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(500)).getConstraints  // off, morning - turn on
  //  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(1400)).getConstraints // off, evening - no sol

  //  val problem = genScheds(100 to 200, "time",0,true) ++ // some will display
  //    genScheds(500 to 600, "time",0,false) ++            // and some will turn on
  //    new GCWriter("time",0,List(500)).getConstraints        // (it is morning)



  val time = System.currentTimeMillis()
  //  val time = System.nanoTime()
  val res = problem.solveBool
  val spent = System.currentTimeMillis() - time

  if (res.isDefined) println("solved in "+spent+" ms.")
  else println("no solution (in "+spent+" ms)")

  if (res.isDefined) println(res.get)
  if (res.isDefined) {
    val pEval = problem.partialEval(res.get)
    println("partial eval: "+pEval)
    pEval.quotient()
    println("quotient:     "+pEval)
    pEval.applyDataAssgn(res.get)
    println("dataAssign:   "+pEval)
    pEval.solveSimpleData(res.get,problem.getDA)
    println("solveData:    "+pEval)
    //    val mapRest    = problem.applyDataAssgn(res.get)
//    val newMapRest = problem.solveSimpleData(res.get,mapRest._1,mapRest._2)
//    val newSol     = problem.incrementAndSolve(cnf._1,cnf._2,res.get,newMapRest._1,newMapRest._2)
//
//    println("assignVars: "+mapRest)
//    println("solveSimpleData: "+newMapRest)
//    if (newSol.isDefined) println("incrementalSol:\n"+newSol.get.pretty)
  }



  class Morning extends IntPredicate {
    // from 7am to 10am
    val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,420),Choco.leq(x,600))
    val funPred = (x:Int) => (x >= 40) && (x <= 600)
    override def toString = "Morning"

    val z3Pred = null
  }
  class Evening extends IntPredicate {
    // from 7pm to midnight
    val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,1140),Choco.leq(x,1440))
    val funPred = (x:Int) => (x >= 1140) && (x <= 1440)
    override def toString = "Evening"

    val z3Pred = null
  }
}
