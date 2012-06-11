package common.beh.benchmarks

import common.beh.Utils._
import common.beh.guardedcommands._
import common.beh.choco.dataconnectors.Predicate
import choco.kernel.model.variables.integer.IntegerVariable
import choco.Choco
import connectors._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:05
 * To change this template use File | Settings | File Templates.
 */

class GCSchedules // needed for the App below to exist

object GCSchedules extends App {

  val morning = new Morning
  val evening  = new Evening

  def genSched(i:Int,on: Boolean): GuardedCommands = {

//    new GCWriter("x",i,List(500)).constraints ++
    val res =
      new GCExRouter("x","a","b",i).constraints ++
      new GCFilter("a","e",i,Neg(Pred(evening,dataVar("a",i)))).constraints ++
      new GCFilter("a","f",i,Pred(evening,dataVar("a",i))).constraints ++
      new GCFilter("b","g",i,Pred(morning,dataVar("b",i))).constraints ++
      new GCMerger("e","g","m",i).constraints ++
      new GCSDrain("a","c",i).constraints ++
      new GCSDrain("b","d",i).constraints ++
      new GCSDrain("g","b",i).constraints ++
      new GCSync("e","disp",i).constraints ++
      new GCSync("f","off",i).constraints ++
      new GCSync("g","on",i).constraints ++
      GuardedCommands(True --> SGuard(Var(flowVar("x",i))))

    if (on) res ++
      new GCSyncFifo("m","c",Some(0),i).constraints ++
      new GCFifo("f","d",None,i).constraints
    else res ++
      new GCSyncFifo("m","c",None,i).constraints ++
      new GCFifo("f","d",Some(0),i).constraints
  }


  def genScheds(uids: Iterable[Int], startVar: String, startUid: Int, on: Boolean): GuardedCommands = {
    var res = new GuardedCommands()
    for (i <- uids) {
      res ++= genSched(i,on)
      // manual replicator from (startVar.startUid) to (x,i)
      val av = Var(flowVar(startVar,startUid))
      val bv = Var(flowVar("x",i))
      res ++= GuardedCommands(Set(
        True --> SGuard(av <-> bv),
        av --> VarAssgn(dataVar("x",i), dataVar(startVar,startUid))
      ))
    }
    res
  }



//  val problem = genSched(0,true)  ++ new GCWriter("x",0,List(500)).constraints ++ // on, morning  - display
//                GuardedCommands(True --> SGuard(Var(flowVar("e",0))))

//  val schedule = genSched(0,true)  ++ new GCWriter("x",0,List(1400)).constraints // on, evening  - turn off
//  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(500)).constraints  // off, morning - turn on
//  val schedule = genSched(0,false) ++ new GCWriter("x",0,List(1400)).constraints // off, evening - no sol

  val problem = genScheds(100 to 200, "time",0,true) ++ // some will display
    genScheds(500 to 600, "time",0,false) ++            // and some will turn on
    new GCWriter("time",0,List(500)).constraints        // (it is morning)



  val time = System.currentTimeMillis()
//  val time = System.nanoTime()
  val res = problem.solve
  val spent = System.currentTimeMillis() - time

  if (res.isDefined) println("solved in "+spent+" ms.")
  else println("no solution (in "+spent+" ms)")
  //  if (res.isDefined) println(res.get.pretty)
//  if (res.isDefined) println("partial eval: "+problem.partialEval(res.get))



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
