package common.beh.guardedcommands

import connectors.GCFilter
import org.scalatest.FunSpec
import common.beh.choco.dataconnectors._
import common.beh.Utils._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 16:42
 * To change this template use File | Settings | File Templates.
 */

class TestDA extends FunSpec {

  describe ("Choco - Writer, 2 Filters (>5, even), and a reader.") {

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

    def evend(x:String) = Pred(even,dataVar(x,0))
    def oddd(x:String) = Pred(odd,dataVar(x,0))

    // TEST
    val c1= new GCFilter("a","b",0,evend("a")).constraints ++
            new GCFilter("b","c",0,oddd("b")).constraints ++
            GuardedCommands(True --> DataAssgn(dataVar("a",0),2)) ++
            GuardedCommands(True --> SGuard(Var(flowVar("c",0))))


    val c2= new GCFilter("a","b",0,evend("a")).constraints ++
            new GCFilter("b","c",0,evend("b")).constraints ++
            GuardedCommands(True --> DataAssgn(dataVar("a",0),6)) ++
            GuardedCommands(True --> SGuard(Var(flowVar("c",0))))

    val c3= new GCFilter("a","b",0,oddd("a")).constraints ++
            new GCFilter("b","c",0,evend("b")).constraints ++
            GuardedCommands(True --> DataAssgn(dataVar("a",0),3)) ++
            GuardedCommands(True --> SGuard(Var(flowVar("b",0))))

//    println(c1.commands)

    val res1 = c1.solveBool
    val res2 = c2.solveBool
    val res3 = c3.solveBool
    if (res3.isDefined) println("solved:\n"+res3.get.pretty)
    else println("no solution")
    println("partial eval: "+c2.partialEval(res2.get))

    it ("c1 should have no sol") {assert (!res1.isDefined)}
    it ("c2 should have sol") {assert (res2.isDefined)}
    it ("c3 should have sol") {assert (res3.isDefined)}


  }

}
