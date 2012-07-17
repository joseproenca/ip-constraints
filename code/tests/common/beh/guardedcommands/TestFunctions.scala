package common.beh.guardedcommands

import common.beh.{Odd, Even}
import common.beh.Utils._
import common.beh.Double
import dataconnectors.{GCTransf, GCFilter}
import org.scalatest.FunSpec

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/07/12
 * Time: 15:20
 * To change this template use File | Settings | File Templates.
 */

class TestFunctions extends FunSpec {

  describe ("Choco - Transformer (x2), replicator, 2 Filters (odd/even).") {

    //    val w: GCBehaviour = new GCWriter("w",42,List(7,5,6,7,8))
    //    val f1: GCBehaviour = new GCFilter("fi","fo",43,(new GT(5)))
    //    val f2: GCBehaviour = new GCFilter("fi","fo",44,(new Even))
    //    val rd: GCBehaviour = new GCReader("r",45,5)
    //
    //
    //    var c = w.constraints ++ f1.constraints ++ f2.constraints ++ rd.constraints
    //    w.connections += f1 -> Set(("w","fi",43))
    //    f1.connections += f2 -> Set(("fo","fi",44))
    //    f2.connections += rd -> Set(("fo","r",45))
    //    c = w.sync(f1,c)
    //    c = f1.sync(f2,c)
    //    c = f2.sync(rd,c)

    val even = new Even
    val odd = new Odd

    def println(s:String) = System.out.println(s)

    def evend(x:String) = IntPred(dataVar(x,0),even)
    def oddd(x:String) = IntPred(dataVar(x,0),odd)

    // TEST
    val c1= new GCTransf("a","b",0,new Double()).constraints ++
      new GCFilter("b","c",0,evend("b")).constraints ++
      new GCFilter("b","d",0,evend("b")).constraints ++
      GuardedCommands(True --> IntAssgn(dataVar("a",0),3)) ++
      GuardedCommands(True --> SGuard(Var(flowVar("a",0))))

    val c2= new GCTransf("a","b",0,new Double()).constraints ++
      new GCFilter("b","c",0,oddd("b")).constraints ++
      new GCFilter("b","d",0,evend("b")).constraints ++
      GuardedCommands(True --> IntAssgn(dataVar("a",0),3)) ++
      GuardedCommands(True --> SGuard(Var(flowVar("a",0))))

    val c3= new GCTransf("a","b",0,new Double()).constraints ++
      new GCFilter("b","c",0,oddd("b")).constraints ++
      new GCFilter("b","d",0,oddd("b")).constraints ++
      GuardedCommands(True --> IntAssgn(dataVar("a",0),3)) ++
      GuardedCommands(True --> SGuard(Var(flowVar("a",0))))

    //    println(c1.commands)

    val res1 = c1.solve
    val res2 = c2.solve
    val res3 = c3.solve
//    val res4 = c4.solve
//    val res4p= c4.solveBool
//
    if (res1.isDefined) println("solved 1:\n"+res1.get.pretty)
    else println("no solution")

    if (res2.isDefined) println("solved 2:\n"+res2.get.pretty)
    else println("no solution")

    if (res3.isDefined) println("solved 3:\n"+res3.get.pretty)
    else println("no solution")

//    println("partial eval 1: "+c1.partialEval(res1.get))
//
//    if (res4.isDefined) println("solved 4:\n"+res4p.get.pretty)
//    else println("no solution")
//    println("partial eval 4: "+c4.partialEval(res4p.get))
//
    it ("c1 should have sol") {assert (res1.isDefined)}
    it ("c2 should have sol") {assert (res2.isDefined)}
    it ("c3 should have sol") {assert (res3.isDefined)}

    it ("c1 should have flow on both filters")
    {assert (res1.get.hasFlow(flowVar("c",0)) && res1.get.hasFlow(flowVar("d",0)))}
    it ("c2 should have flow on 2nd filter with data 6")
    {assert (!res2.get.hasFlow(flowVar("c",0)) && res2.get(dataVar("d",0))==6)}
    it ("c3 should have no flow on both filters")
    {assert (!res3.get.hasFlow(flowVar("c",0)) && !res3.get.hasFlow(flowVar("d",0)))}


    val const = c2

    println("GC:\n"+const.commands.mkString("\n"))

    val cnf = const.toCNF
//    println("cnf: "+cnf)

    println("--\nDA:\n"+const.da.pp)


    val optSolBool = const.solveBool(cnf._1,cnf._2)
//    if (!optSolBool.isDefined)
        println("--\ninit guess:\n"+optSolBool.get.pretty)
//      return Some(new GCSolution(optSolBool.get,Map[String, Int]()))

    val pEval = const.partialEval(optSolBool.get)
    val solBool = optSolBool.get
        println("#> solved  pEval             - "+pEval)
    pEval.quotient()
        println("#> calculated quotient       - "+pEval)
    pEval.applyDataAssgn(solBool)
        println("#> solved simple data assign - "+pEval)
//    if (pEval.isFinished)
//      println("#> Finished - "+ pEval.getSol(solBool))
//    pEval.freshTraversal()
//    println("#> calculated freshTraversal     - "+pEval)

  }

}
