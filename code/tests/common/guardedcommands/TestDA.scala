package common.guardedcommands

import org.scalatest.FunSpec
import common.{Utils}
import Utils._
import common.guardedcommands.dataconnectors.{GCSync, GCFilter}
import common.examples.{Odd, Even}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 16:42
 * To change this template use File | Settings | File Templates.
 */

class TestDA extends FunSpec {

  describe ("GC - Writer, 2 Filters (>5, even), and a reader.") {

//    val w: GCBehaviour = new GCWriter("w",42,List(7,5,6,7,8))
//    val f1: GCBehaviour = new GCFilter("fi","fo",43,(new GT(5)))
//    val f2: GCBehaviour = new GCFilter("fi","fo",44,(new Even))
//    val rd: GCBehaviour = new GCReader("r",45,5)
//
//
//    var c = w.getConstraints ++ f1.getConstraints ++ f2.getConstraints ++ rd.getConstraints
//    w.connections += f1 -> Set(("w","fi",43))
//    f1.connections += f2 -> Set(("fo","fi",44))
//    f2.connections += rd -> Set(("fo","r",45))
//    c = w.sync(f1,c)
//    c = f1.sync(f2,c)
//    c = f2.sync(rd,c)

    val even = new Even
    val odd = new Odd

    def evend(x:String) = IntPred(dataVar(x,0),even)
    def oddd(x:String) = IntPred(dataVar(x,0),odd)

    // TEST
    // data fails first filter, second should be lazy using chocoSAT. No flow on "c", so fail.
    val c1= new GCFilter("a","b",0,oddd("a")).getConstraints ++
            new GCFilter("b","c",0,evend("b")).getConstraints ++
            Formula(True --> IntAssgn(dataVar("a",0),2)) ++
            Formula(True --> (Var(flowVar("c",0))))
    // Result is correct, but it is ALWAYS checking all predicates!

    // both predicate hold - requirement for at least an end with flow yields dataflow everywhere
    val c2= new GCFilter("a","b",0,evend("a")).getConstraints ++
            new GCFilter("b","c",0,evend("b")).getConstraints ++
            Formula(True --> IntAssgn(dataVar("a",0),6)) //++
//            Formula(True --> SGuard((Var(flowVar("a",0)))))

    // as c1, but data flow only on "a" and is discarded (no requirement to flow on "c").
    val c3= new GCFilter("a","b",0,oddd("a")).getConstraints ++
            new GCFilter("b","c",0,evend("b")).getConstraints ++
            Formula(True --> IntAssgn(dataVar("a",0),2)) //++
//            Formula(True --> SGuard(Var(flowVar("b",0))))

    // control test with no data filters.
    val c4= new GCSync("a","b",0).getConstraints ++
            new GCSync("b","c",0).getConstraints ++
            Formula(True --> IntAssgn(dataVar("a",0),3))// ++
//            Formula(True --> SGuard(Var(flowVar("b",0))))

    //    println(c1.commands)

    val res1 = c1.solveBool
    val res2 = c2.solveBool
    val res3 = c3.solveBool
    val res4 = c4.solveBool
    val resB= c3.solveChocoBool

    println("-----------\n"+c1.commands.mkString("\n"))
    println("-----------")


    if (res1.isDefined) print("solved 1:\n"+res1.get)
    else println("1: no solution")

    if (res2.isDefined) print("solved 2:\n"+res2.get)
    else println("2: no solution")

    if (res2.isDefined) println("partial eval 2: "+c2.partialEval(res2.get))

    if (res3.isDefined) print("solved 3:\n"+res3.get)
    else println("3: no solution")

    if (res4.isDefined) print("solved 4:\n"+res4.get)
    else println("4: no solution")

    println("partial eval 4: "+c4.partialEval(res4.get))

    if (resB.isDefined) print("solved B:\n"+resB.get)
    else println("B: no solution")

    it ("c1 should have no sol") {assert (!res1.isDefined)}
    it ("c2 should have sol") {assert (res2.isDefined)}
    it ("c3 should have sol") {assert (res3.isDefined)}
    it ("c4 should have sol") {assert (res4.isDefined)}


  }

}
