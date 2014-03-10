package reopp.common.guardedcommands

import reopp.common.{Utils}
import Utils._
import org.scalatest.FunSpec
import reopp.common.guardedcommands.dataconnectors.GCIFilter
import reopp.common.examples.{LT, GT}

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

    def lfivex(x:String) = lfive //IntPred(mkDataVar(x),lfive)
    def gtwox(x:String) = gtwo //IntPred(mkDataVar(x),gtwo)

    println("--- data provided - no CSP solving ---")
    val c1= new GCIFilter("a","b",lfivex("a")).getConstraints ++
      new GCIFilter("b","c",gtwox("b")).getConstraints ++
      Formula(True --> IntAssgn(mkDataVar("a"),3)) ++
      Formula(True --> Var(mkFlowVar("b")))
    val res1 = c1.solveIterative
    if (res1.isDefined) println("solved:\n"+res1.get)
    else println("no sol")


    println("--- data not provided - CSP has a sol ---")
    val c2= new GCIFilter("a","b",lfivex("a")).getConstraints ++
      new GCIFilter("b","c",gtwox("b")).getConstraints ++
      Formula(True --> Var(mkFlowVar("b")))
    val res2 = c2.solveIterative
    if (res2.isDefined) println("solved:\n"+res2.get)
    else println("no sol")


    println("--- data not provided - CSP needs reiteration ---")
    val c3= new GCIFilter("a","b",lfivex("a")).getConstraints ++
      new GCIFilter("b","c",gtwox("b")).getConstraints ++
      Formula(True --> Var(mkFlowVar("a")))
    val res3 = c3.solveIterative
    if (res3.isDefined) println("solved:\n"+res3.get)
    else println("no sol")


    // using CHOCO

    println("--- data provided - no CSP solving ---")
    val c4= new GCIFilter("a","b",lfivex("a")).getConstraints ++
      new GCIFilter("b","c",gtwox("b")).getConstraints ++
      Formula(True --> IntAssgn(mkDataVar("a"),3)) ++
      Formula(True --> Var(mkFlowVar("b")))
    val res4 = c4.solveChocoSat
    if (res4.isDefined) println("solved:\n"+res4.get)
    else println("no sol")

    println("--- data not provided - CSP has a sol ---")
    val c5= new GCIFilter("a","b",lfivex("a")).getConstraints ++
      new GCIFilter("b","c",gtwox("b")).getConstraints ++
      Formula(True --> Var(mkFlowVar("b")))
    val res5 = c5.solveChocoSat
    if (res5.isDefined) println("solved:\n"+res5.get)
    else println("no sol")

    println("--- data not provided - CSP needs reiteration ---")
    val c6= new GCIFilter("a","b",lfivex("a")).getConstraints ++
      new GCIFilter("b","c",gtwox("b")).getConstraints ++
      Formula(True --> Var(mkFlowVar("a")))
    val res6 = c6.solveChocoSat
    if (res6.isDefined) println("solved:\n"+res6.get)
    else println("no sol")


    ///// tests ////
    it ("c1 should have sol") {
      assert (res1.isDefined)
      assert (res1.get.apply(mkDataVar("a")) == 3)
      assert (res1.get.apply(mkDataVar("b")) == 3)
      assert (res1.get.apply(mkDataVar("c")) == 3)
    }

    it ("c2 should have sol") {
      assert (res2.isDefined)
      val sol = res2.get
      assert (sol(mkDataVar("a")).isInstanceOf[Int])
      assert (sol(mkDataVar("b")).isInstanceOf[Int])
      assert (sol(mkDataVar("a")).asInstanceOf[Int] < 5)
      assert (if (sol.hasFlowOn(mkFlowVar("c")))
        sol(mkDataVar("b")).asInstanceOf[Int] > 2
      else sol(mkDataVar("b")).asInstanceOf[Int] <= 2)
    }

    it ("c3 should have sol") {
      assert (res3.isDefined)
      val sol = res3.get
      assert (if (sol.hasFlowOn(mkFlowVar("b"))) sol(mkDataVar("b")).asInstanceOf[Int] < 5
      else                             sol(mkDataVar("a")).asInstanceOf[Int] >= 5)
      assert (if (sol.hasFlowOn(mkFlowVar("c"))) sol(mkDataVar("c")).asInstanceOf[Int] > 2
      else if (sol.hasFlowOn(mkFlowVar("b"))) sol(mkDataVar("b")).asInstanceOf[Int] <= 2
      else true)
    }

    it ("c4 should have sol") {
      assert (res4.isDefined)
      assert (res4.get.apply(mkDataVar("a")) == 3)
      assert (res4.get.apply(mkDataVar("b")) == 3)
      assert (res4.get.apply(mkDataVar("c")) == 3)
    }

    it ("c5 should have sol") {
      assert (res5.isDefined)
      val sol = res5.get
      assert (sol(mkDataVar("a")).asInstanceOf[Int] < 5)
      assert (if (sol.hasFlowOn(mkFlowVar("c")))
        sol(mkDataVar("b")).asInstanceOf[Int] > 2
      else sol(mkDataVar("b")).asInstanceOf[Int] <= 2)
    }

    it ("c6 should have sol") {
      assert (res6.isDefined)
      val sol = res6.get
      assert (if (sol.hasFlowOn(mkFlowVar("b"))) sol(mkDataVar("b")).asInstanceOf[Int] < 5
      else                             sol(mkDataVar("a")).asInstanceOf[Int] >= 5)
      assert (if (sol.hasFlowOn(mkFlowVar("c"))) sol(mkDataVar("c")).asInstanceOf[Int] > 2
      else if (sol.hasFlowOn(mkFlowVar("b"))) sol(mkDataVar("b")).asInstanceOf[Int] <= 2
      else true)
    }

  }
}
