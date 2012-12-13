package common.benchmarks

import common.guardedcommands._
import dataconnectors._
import common.{Utils, IntPredicate}
import Utils._
import common.guardedcommands.Neg
import common.guardedcommands.VarAssgn
import common.guardedcommands.IntPred
import scala.Some
import common.guardedcommands.Var
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import common.IntPredicate

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 20/06/12
 * Time: 09:25
 * To change this template use File | Settings | File Templates.
 */

object Warmup {
  def go {
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


    val morning = new Morning
    val evening  = new Evening

    def genSched(i:Int,on: Boolean): GuardedCommands = {
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
        new GCSyncFifo("m","c",Some(0),i).getConstraints ++
        new GCFifo("f","d",None,i).getConstraints
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


    for (n <- 25 to 30) {
      val n2 = n / 2

      val problem = genScheds(1 to n2, "time",0,on = true) ++   // some will display
        genScheds(n2+1 to n, "time",0,on = false) ++            // and some will turn on
        new GCWriter("time",0,List(500)).getConstraints ++    // (it is morning)
        GuardedCommands(True --> Var(flowVar("time",0))) // require some dataflow

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
