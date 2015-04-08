package reopp.common.guardedcommands

import org.scalatest.FunSpec
import reopp.common.Utils._
import reopp.common.guardedcommands.dataconnectors.{GCIFilter, GCTransf}
import reopp.common.examples.{Double, LT, GT}

/**
 * Test channels with functions in guarded commands.
 *
 * Created by jose on 09/07/12.
 */
class TestFunctIteration extends FunSpec {

  describe ("GC - [x2].Writer.[<5].[>2].") {

    val lfive = new LT(5)
    val gtwo= new GT(2)
    val double = new Double

    def lfivex(x:String) = IntPred(mkDataVar(x),lfive)
    def gtwox(x:String) = IntPred(mkDataVar(x),gtwo)


    println("--- 1: data provided - no CSP solving ---")
    val c1 =
      new GCTransf("z","a",double).getConstraints ++
      new GCIFilter("a","b",lfive).getConstraints ++
      new GCIFilter("b","c",gtwo).getConstraints ++
      new GCTransf("c","d",double).getConstraints ++
      Formula(True --> IntAssgn(mkDataVar("z"),2)) ++
      Formula(True --> Var(mkFlowVar("z")))
    val res1 = c1.solveIterative
    if (res1.isDefined) println("solved:\n"+res1.get)
    else println("no sol")


    println("--- 2: data not provided - CSP has a sol ---")
    val c2=
      new GCTransf("z","a",double).getConstraints ++
      new GCIFilter("a","b",lfive).getConstraints ++
      new GCIFilter("b","c",gtwo).getConstraints ++
      new GCTransf("c","d",double).getConstraints ++
      Formula(True --> Var(mkFlowVar("b")))
    val res2 = c2.solveIterative
    if (res2.isDefined) println("solved:\n"+res2.get)
    else println("no sol")


    println("--- 3: data not provided - CSP needs reiteration ---")
    val c3=
      new GCTransf("z","a",double).getConstraints ++
      new GCIFilter("a","b",lfive).getConstraints ++
      new GCIFilter("b","c",gtwo).getConstraints ++
      new GCTransf("c","d",double).getConstraints ++
      Formula(True --> Var(mkFlowVar("a")))
    val res3 = c3.solveIterative
    if (res3.isDefined) println("solved:\n"+res3.get)
    else println("no sol")


    val res1b = c1.quickDataSolve
    if (res1b.isDefined) println("quickly solved:\n"+res1b.get)
    else println("no sol")

    val res2b = c2.quickDataSolve
    if (res2b.isDefined) println("quickly solved:\n"+res2b.get)
    else println("no sol")

    val res3b = c3.quickDataSolve
    if (res3b.isDefined) println("quickly solved:\n"+res3b.get)
    else println("no sol")




    //    val const = c3
//
//    println("GC:\n"+const.commands.mkString("\n"))
//
//    val cnf = const.toCNF
//    //    println("cnf: "+cnf)
//
//    println("--\nDA:\n"+const.da.pp)
//
//    val optSolBool = const.solveBool(cnf._1,cnf._2)
//    //    if (!optSolBool.isDefined)
//    println("--\ninit guess:\n"+optSolBool.get.pretty)
//    //      return Some(new GCSolution(optSolBool.get,Map[String, Int]()))
//
//    val pEval = const.partialEval(optSolBool.get)
//    val solBool = optSolBool.get
//    println("#> solved  pEval             - "+pEval)
//    pEval.quotient()
//    println("#> calculated quotient       - "+pEval)
//    pEval.applyDataAssgn(solBool)
//    println("#> solved simple data assign - "+pEval)
//    if (pEval.isFinished)
//      println("#> Finished - "+ pEval.getSol(solBool))
////    pEval.freshTraversal()
////    println("#> calculated freshTraversal     - "+pEval)
//    pEval.solveSimpleData(solBool,const.da)
//    println("#> solved domain elements    - "+pEval)
//    if (pEval.isFinished)
//      println("#> Finished - "+ pEval.getSol(solBool))
//    val (optSol2,newcnf) = const.incrementAndSolve(cnf._1,cnf._2,solBool,pEval)
//    println("#> incremented and solved new constr.")//+newcnf)
//    if (!optSol2.isDefined)
//      println("#> No solution")
//    else
//      println("#> need to restart loop: "+const.partialEval(optSol2.get)+"\n"+optSol2.get.pretty)




    // using CHOCO

    println("--- 4: data provided (SMT) ---")
    val c4=
      new GCTransf("z","a",double).getConstraints ++
      new GCIFilter("a","b",lfive).getConstraints ++
      new GCIFilter("b","c",gtwo).getConstraints ++
      new GCTransf("c","d",double).getConstraints ++
      Formula(True --> IntAssgn(mkDataVar("z"),2)) ++
      Formula(True --> Var(mkFlowVar("b")))
    val res4 = c4.solveChoco
    if (res4.isDefined) println("solved:\n"+res4.get)
    else println("no sol")

//    println("--- data not provided - CSP has a sol ---")
//    val c5=
//      new GCIFilter("a","b",lfivex("a")).getConstraints ++
//      new GCIFilter("b","c",gtwox("b")).getConstraints ++
//      Formula(True --> SGuard(Var(mkFlowVar("b"))))
//    val res5 = c5.solveChocoSat
//    if (res5.isDefined) println("solved:\n"+res5.get.pretty)
//    else println("no sol")

    println("--- 6: data not provided (SMT)  ---")
    val c6=
      new GCTransf("z","a",double).getConstraints ++
      new GCIFilter("a","b",lfive).getConstraints ++
      new GCIFilter("b","c",gtwo).getConstraints ++
      new GCTransf("c","d",double).getConstraints ++
      Formula(True --> Var(mkFlowVar("d")))
    val res6 = c6.solveChoco
    if (res6.isDefined) println("solved:\n"+res6.get)
    else println("no sol")


    ///// tests ////
    it ("c1 should have sol") {
      assert (res1.isDefined)
      assert (res1.get.apply(mkDataVar("z")) == 2)
      assert (res1.get.apply(mkDataVar("a")) == 4)
      assert (res1.get.apply(mkDataVar("b")) == 4)
      assert (res1.get.apply(mkDataVar("c")) == 4)
      assert (res1.get.apply(mkDataVar("d")) == 8)
    }

    it ("c2 should have sol") {
      assert (res2.isDefined)
      val sol = res2.get
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

    it ("c4 should have sol (SMT)") {
      assert (res4.isDefined)
      assert (res4.get.apply(mkDataVar("z")) == 2)
      assert (res4.get.apply(mkDataVar("a")) == 4)
      assert (res4.get.apply(mkDataVar("b")) == 4)
      assert (res4.get.apply(mkDataVar("c")) == 4)
      assert (res4.get.apply(mkDataVar("d")) == 8)
    }

//    it ("c5 should have sol") {
//      assert (res5.isDefined)
//      val sol = res5.get
//      assert (sol(mkDataVar("a")) < 5)
//      assert (if (sol.hasFlow(mkFlowVar("c")))
//        sol(mkDataVar("b")) > 2
//      else sol(mkDataVar("b")) <= 2)
//    }

    it ("c6 should have sol (SMT)") {
      assert (res6.isDefined)
      val sol = res6.get
      assert (if (sol.hasFlowOn(mkFlowVar("b"))) sol.getVal(mkDataVar("b")).get < 5
      else                             sol.getVal(mkDataVar("a")).get >= 5)
      assert (if (sol.hasFlowOn(mkFlowVar("c"))) sol.getVal(mkDataVar("c")).get > 2
      else                             sol.getVal(mkDataVar("b")).get <= 2)
    }

  }
}
