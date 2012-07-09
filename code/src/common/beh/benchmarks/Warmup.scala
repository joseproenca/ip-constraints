package common.beh.benchmarks

import common.beh.guardedcommands._
import dataconnectors._
import common.beh.Utils._
import common.beh.guardedcommands.Neg
import common.beh.guardedcommands.VarAssgn
import common.beh.guardedcommands.Pred
import scala.Some
import common.beh.guardedcommands.SGuard
import common.beh.guardedcommands.Var
import common.beh.Predicate
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 20/06/12
 * Time: 09:25
 * To change this template use File | Settings | File Templates.
 */

object Warmup {
  def go {
    class Morning extends Predicate {
      // from 7am to 10am
      val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,420),Choco.leq(x,600))
      val funPred = (x:Int) => (x >= 40) && (x <= 600)
      override def toString = "Morning"
    }
    class Evening extends Predicate {
      // from 7pm to midnight
      val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,1140),Choco.leq(x,1440))
      val funPred = (x:Int) => (x >= 1140) && (x <= 1440)
      override def toString = "Evening"
    }


    val morning = new Morning
    val evening  = new Evening

    def genSched(i:Int,on: Boolean): GuardedCommands = {
      val res =
        new GCExRouter("x","a","b",i).constraints ++
        new GCFilter("a","e",i,Neg(Pred(dataVar("a",i),evening))).constraints ++
        new GCFilter("a","f",i,Pred(dataVar("a",i),evening)).constraints ++
        new GCFilter("b","g",i,Pred(dataVar("b",i),morning)).constraints ++
        new GCMerger("e","g","m",i).constraints ++
        new GCSDrain("a","c",i).constraints ++
        new GCSDrain("b","d",i).constraints ++
        new GCSDrain("g","b",i).constraints ++
        new GCSync("e","disp",i).constraints ++
        new GCSync("f","off",i).constraints ++
        new GCSync("g","on",i).constraints ++
        GuardedCommands(True --> SGuard(Var(flowVar("x",i))))
        new GCSyncFifo("m","c",Some(0),i).constraints ++
        new GCFifo("f","d",None,i).constraints
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


    for (n <- 25 to 30) {
      val n2 = n / 2

      val problem = genScheds(1 to n2, "time",0,true) ++   // some will display
        genScheds(n2+1 to n, "time",0,false) ++            // and some will turn on
        new GCWriter("time",0,List(500)).constraints ++    // (it is morning)
        GuardedCommands(True --> SGuard(Var(flowVar("time",0)))) // require some dataflow

      val time = System.currentTimeMillis()
      val res = problem.solveChocoSat
      val spent = System.currentTimeMillis() - time

      val time2 = System.currentTimeMillis()
      val res2 = problem.solve
      val spent2 = System.currentTimeMillis() - time2

//      if (res.isDefined) print(spent+" ")
//      else print("- ")
//      if (res2.isDefined) print(spent2+" ")
//      else print("- ")
//      println("")
    }


  }
}
