package common.guardedcommands

import common.{Utils}
import Utils._
import org.scalatest.FunSpec
import common.guardedcommands.dataconnectors.GCFilter
import common.examples.{LT, GT}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/06/12
 * Time: 17:22
 * To change this template use File | Settings | File Templates.
 */

class TestSolveLoop extends FunSpec {

  describe ("GC - Writer, 2 Filters (<5, >2), and a reader.") {

    val lfive = new LT(5)
    val gtwo= new GT(2)

    def lfivex(x:String) = IntPred(dataVar(x,0),lfive)
    def gtwox(x:String) = IntPred(dataVar(x,0),gtwo)

    println("--- data provided - no CSP solving ---")
    val c1= new GCFilter("a","b",0,lfivex("a")).getConstraints ++
      new GCFilter("b","c",0,gtwox("b")).getConstraints ++
      GuardedCommands(True --> IntAssgn(dataVar("a",0),3)) ++
      GuardedCommands(True --> Var(flowVar("b",0)))
    val res1 = c1.solve
    if (res1.isDefined) println("solved:\n"+res1.get)
    else println("no sol")


    println("--- data not provided - CSP has a sol ---")
    val c2= new GCFilter("a","b",0,lfivex("a")).getConstraints ++
      new GCFilter("b","c",0,gtwox("b")).getConstraints ++
      GuardedCommands(True --> Var(flowVar("b",0)))
    val res2 = c2.solve
    if (res2.isDefined) println("solved:\n"+res2.get)
    else println("no sol")


    println("--- data not provided - CSP needs reiteration ---")
    val c3= new GCFilter("a","b",0,lfivex("a")).getConstraints ++
      new GCFilter("b","c",0,gtwox("b")).getConstraints ++
      GuardedCommands(True --> Var(flowVar("a",0)))
    val res3 = c3.solve
    if (res3.isDefined) println("solved:\n"+res3.get)
    else println("no sol")


    // using CHOCO

    println("--- data provided - no CSP solving ---")
    val c4= new GCFilter("a","b",0,lfivex("a")).getConstraints ++
      new GCFilter("b","c",0,gtwox("b")).getConstraints ++
      GuardedCommands(True --> IntAssgn(dataVar("a",0),3)) ++
      GuardedCommands(True --> Var(flowVar("b",0)))
    val res4 = c4.solveChocoSat
    if (res4.isDefined) println("solved:\n"+res4.get)
    else println("no sol")

    println("--- data not provided - CSP has a sol ---")
    val c5= new GCFilter("a","b",0,lfivex("a")).getConstraints ++
      new GCFilter("b","c",0,gtwox("b")).getConstraints ++
      GuardedCommands(True --> Var(flowVar("b",0)))
    val res5 = c5.solveChocoSat
    if (res5.isDefined) println("solved:\n"+res5.get)
    else println("no sol")

    println("--- data not provided - CSP needs reiteration ---")
    val c6= new GCFilter("a","b",0,lfivex("a")).getConstraints ++
      new GCFilter("b","c",0,gtwox("b")).getConstraints ++
      GuardedCommands(True --> Var(flowVar("a",0)))
    val res6 = c6.solveChocoSat
    if (res6.isDefined) println("solved:\n"+res6.get)
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
      assert (sol(dataVar("a",0)).isInstanceOf[Int])
      assert (sol(dataVar("b",0)).isInstanceOf[Int])
      assert (sol(dataVar("a",0)).asInstanceOf[Int] < 5)
      assert (if (sol.hasFlowOn(flowVar("c",0)))
        sol(dataVar("b",0)).asInstanceOf[Int] > 2
      else sol(dataVar("b",0)).asInstanceOf[Int] <= 2)
    }

    it ("c3 should have sol") {
      assert (res3.isDefined)
      val sol = res3.get
      assert (if (sol.hasFlowOn(flowVar("b",0))) sol(dataVar("b",0)).asInstanceOf[Int] < 5
      else                             sol(dataVar("a",0)).asInstanceOf[Int] >= 5)
      assert (if (sol.hasFlowOn(flowVar("c",0))) sol(dataVar("c",0)).asInstanceOf[Int] > 2
      else if (sol.hasFlowOn(flowVar("b",0))) sol(dataVar("b",0)).asInstanceOf[Int] <= 2
      else true)
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
      assert (sol(dataVar("a",0)).asInstanceOf[Int] < 5)
      assert (if (sol.hasFlowOn(flowVar("c",0)))
        sol(dataVar("b",0)).asInstanceOf[Int] > 2
      else sol(dataVar("b",0)).asInstanceOf[Int] <= 2)
    }

    it ("c6 should have sol") {
      assert (res6.isDefined)
      val sol = res6.get
      assert (if (sol.hasFlowOn(flowVar("b",0))) sol(dataVar("b",0)).asInstanceOf[Int] < 5
      else                             sol(dataVar("a",0)).asInstanceOf[Int] >= 5)
      assert (if (sol.hasFlowOn(flowVar("c",0))) sol(dataVar("c",0)).asInstanceOf[Int] > 2
      else if (sol.hasFlowOn(flowVar("b",0))) sol(dataVar("b",0)).asInstanceOf[Int] <= 2
      else true)
    }

  }
}
