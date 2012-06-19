package common.beh.guardedcommands

import connectors.GCFilter
import common.beh.Utils._
import org.scalatest.FunSpec
import common.beh.choco.dataconnectors.{GT, LT}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/06/12
 * Time: 17:22
 * To change this template use File | Settings | File Templates.
 */

class TestSolveLoop extends FunSpec {

  describe ("Choco - Writer, 2 Filters (<5, >2), and a reader.") {

    val lfive = new LT(5)
    val gtwo= new GT(2)

    def lfivex(x:String) = Pred(lfive,dataVar(x,0))
    def gtwox(x:String) = Pred(gtwo,dataVar(x,0))

    println("--- data provided - no CSP solving ---")
    val c1= new GCFilter("a","b",0,lfivex("a")).constraints ++
      new GCFilter("b","c",0,gtwox("b")).constraints ++
      GuardedCommands(True --> DataAssgn(dataVar("a",0),3)) ++
      GuardedCommands(True --> SGuard(Var(flowVar("b",0))))
    val res1 = c1.solve
    if (res1.isDefined) println("solved:\n"+res1.get.pretty)
    else println("no sol")


    println("--- data not provided - CSP has a sol ---")
    val c2= new GCFilter("a","b",0,lfivex("a")).constraints ++
      new GCFilter("b","c",0,gtwox("b")).constraints ++
      GuardedCommands(True --> SGuard(Var(flowVar("b",0))))
    val res2 = c2.solve
    if (res2.isDefined) println("solved:\n"+res2.get.pretty)
    else println("no sol")


    println("--- data not provided - CSP needs reiteration ---")
    val c3= new GCFilter("a","b",0,lfivex("a")).constraints ++
      new GCFilter("b","c",0,gtwox("b")).constraints ++
      GuardedCommands(True --> SGuard(Var(flowVar("a",0))))
    val res3 = c3.solve
    if (res3.isDefined) println("solved:\n"+res3.get.pretty)
    else println("no sol")


    // using CHOCO

    println("--- data provided - no CSP solving ---")
    val c4= new GCFilter("a","b",0,lfivex("a")).constraints ++
      new GCFilter("b","c",0,gtwox("b")).constraints ++
      GuardedCommands(True --> DataAssgn(dataVar("a",0),3)) ++
      GuardedCommands(True --> SGuard(Var(flowVar("b",0))))
    val res4 = c4.solveChocoSat
    if (res4.isDefined) println("solved:\n"+res4.get.pretty)
    else println("no sol")

    println("--- data not provided - CSP has a sol ---")
    val c5= new GCFilter("a","b",0,lfivex("a")).constraints ++
      new GCFilter("b","c",0,gtwox("b")).constraints ++
      GuardedCommands(True --> SGuard(Var(flowVar("b",0))))
    val res5 = c5.solveChocoSat
    if (res5.isDefined) println("solved:\n"+res5.get.pretty)
    else println("no sol")

    println("--- data not provided - CSP needs reiteration ---")
    val c6= new GCFilter("a","b",0,lfivex("a")).constraints ++
      new GCFilter("b","c",0,gtwox("b")).constraints ++
      GuardedCommands(True --> SGuard(Var(flowVar("a",0))))
    val res6 = c6.solveChocoSat
    if (res6.isDefined) println("solved:\n"+res6.get.pretty)
    else println("no sol")


    ///// tests ////
    it ("c1 should have sol") {
      assert (res1.isDefined)
      assert (res1.get.apply(dataVar("a",0)) == 3)
      assert (res1.get.apply(dataVar("b",0)) == 3)
      assert (res1.get.apply(dataVar("c",0)) == 3)
    }

    it ("c2 should have sol") {
      assert (res2.isDefined)
      val sol = res2.get
      assert (sol(dataVar("a",0)) < 5)
      assert (if (sol.hasFlow(flowVar("c",0)))
        sol(dataVar("b",0)) > 2
      else sol(dataVar("b",0)) <= 2)
    }

    it ("c3 should have sol") {
      assert (res3.isDefined)
      val sol = res3.get
      assert (if (sol.hasFlow(flowVar("b",0))) sol(dataVar("b",0)) < 5
      else                             sol(dataVar("a",0)) >= 5)
      assert (if (sol.hasFlow(flowVar("c",0))) sol(dataVar("c",0)) > 2
      else                             sol(dataVar("b",0)) <= 2)
    }

    it ("c4 should have sol") {
      assert (res4.isDefined)
      assert (res4.get.apply(dataVar("a",0)) == 3)
      assert (res4.get.apply(dataVar("b",0)) == 3)
      assert (res4.get.apply(dataVar("c",0)) == 3)
    }

    it ("c5 should have sol") {
      assert (res5.isDefined)
      val sol = res5.get
      assert (sol(dataVar("a",0)) < 5)
      assert (if (sol.hasFlow(flowVar("c",0)))
        sol(dataVar("b",0)) > 2
      else sol(dataVar("b",0)) <= 2)
    }

    it ("c6 should have sol") {
      assert (res6.isDefined)
      val sol = res6.get
      assert (if (sol.hasFlow(flowVar("b",0))) sol(dataVar("b",0)) < 5
      else                             sol(dataVar("a",0)) >= 5)
      assert (if (sol.hasFlow(flowVar("c",0))) sol(dataVar("c",0)) > 2
      else                             sol(dataVar("b",0)) <= 2)
    }

  }
}
