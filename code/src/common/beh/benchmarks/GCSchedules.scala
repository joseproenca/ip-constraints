package common.beh.benchmarks

import common.beh.Utils._
import common.beh.guardedcommands._
import common.beh.IntPredicate
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import dataconnectors._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:05
 * To change this template use File | Settings | File Templates.
 */

class GCSchedules // needed for the App below to exist

object GCSchedules extends App {

  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
          else               10
  val choco = if (args.size > 1) args(1) startsWith "c"
              else               false
  val justInit = if (args.size > 2) args(2) startsWith "i"
                 else               false

  val morning = new Morning
  val evening  = new Evening

  def genSched(i:Int,on: Boolean): GuardedCommands = {

//    new GCWriter("x",i,List(500)).getConstraints ++
    val res =
      new GCExRouter("x","a","b",i).getConstraints ++
      new GCFilter("a","e",i,Neg(IntPred(dataVar("a",i),evening))).getConstraints ++
      new GCFilter("a","f",i,IntPred(dataVar("a",i),evening)).getConstraints ++
      new GCFilter("b","g",i,IntPred(dataVar("b",i),morning)).getConstraints ++
      new GCMerger("e","g","m",i).getConstraints ++
      new GCSDrain("a","c",i).getConstraints ++
      new GCSDrain("b","d",i).getConstraints ++
      new GCSDrain("g","b",i).getConstraints ++
      new GCSync("e","disp",i).getConstraints ++
      new GCSync("f","off",i).getConstraints ++
      new GCSync("g","on",i).getConstraints ++
      GuardedCommands(True --> Var(flowVar("x",i)))

    if (on) res ++
      new GCSyncFifo("m","c",Some(0),i).getConstraints ++
      new GCFifo("f","d",None,i).getConstraints
    else res ++
      new GCSyncFifo("m","c",None,i).getConstraints ++
      new GCFifo("f","d",Some(0),i).getConstraints
  }


  def genScheds(uids: Iterable[Int], startVar: String, startUid: Int, on: Boolean): GuardedCommands = {
    var res = new GuardedCommands()
    for (i <- uids) {
      res ++= genSched(i,on)
      // manual replicator from (startVar.startUid) to (x,i)
      val av = Var(flowVar(startVar,startUid))
      val bv = Var(flowVar("x",i))
      res ++= GuardedCommands(Set(
        True --> (av <-> bv),
        av --> VarAssgn(dataVar("x",i), dataVar(startVar,startUid))
      ))
    }
    res
  }



//  val problem = genSched(0,true)  ++ new GCWriter("x",0,List(500)).getConstraints ++ // on, morning  - display
//                GuardedCommands(True --> SGuard(Var(flowVar("e",0))))

//  val schedule = genSched(0,true)  ++ new GCWriter("x",0,List(1400)).getConstraints // on, evening  - turn off
//  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(500)).getConstraints  // off, morning - turn on
//  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(1400)).getConstraints // off, evening - no sol

  val n2 = n / 2

  val problem = genScheds(1 to n2, "time",0,on = true) ++   // some will display
    genScheds(n2+1 to n, "time",0,on = false) ++            // and some will turn on
    new GCWriter("time",0,List(500)).getConstraints ++    // (it is morning)
    GuardedCommands(True --> Var(flowVar("time",0))) // require some dataflow

  if (justInit) problem.justInit = true

  if (choco) {
    val time = System.currentTimeMillis()
    val res = problem.solveChocoSat
    val spent = System.currentTimeMillis() - time

    print(spent)

//    if (res.isDefined) println("PAC solved in "+spent+" ms.")
//    else println("PAC - no solution (in "+spent+" ms)")

//    if (res.isDefined) print(spent+" ")
//    else print("- ")
  }
  else {
    val time = System.currentTimeMillis()
    val res = problem.solve
    val spent = System.currentTimeMillis() - time

    print(spent)

//    if (res.isDefined) println("PAS solved in "+spent+" ms.")
//    else println("PAS - no solution (in "+spent+" ms)")

//    if (res.isDefined) print(spent+" ")
//    else print("- ")
  }



//  val time2 = System.currentTimeMillis()
//  val res2 = problem.solveChocoSat
//  val spent2 = System.currentTimeMillis() - time2
//
//  if (res2.isDefined) println("CHO/SAT solved in "+spent2+" ms.")
//  else println("CHO/SAT - no solution (in "+spent2+" ms)")

//  if (res.isDefined) println("sol: "+res.get.pretty)
//  else println("no sol")



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
