package reopp.common.examples

import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import reopp.common.{Solution, Predicate}
import z3.scala.{Z3AST, Z3Context}
import reopp.common.guardedcommands.IntPred
import reopp.common.Utils._
import reopp.common.guardedcommands.IntPred
import scala.Some

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 10/04/13.
 */
object Subtract extends App {

  val subtr = reopp.common.Function("subtract"){
    case n1:Int => 0 - n1
    case List(n1:Int,n2:Int) => n1 - n2
    case List(n1:Int,n2:Int,n3:Int) => (n1 - n2) - n3
    case x => sys.error("wrong arguments for subtract "+x+":"+x.getClass)
  }

  def is(n:Int) = Predicate("(=="+n+")") {
    case x: Int => x == n
  }

//  val connector =
//    writer("a1",List(7)) ++
//    writer("a2",List(4)) ++
////    writer("a3",List(2)) ++
////    writer("b",List(10)) ++
////    nmerger(List("a1","a2","a3"),"a") ++
//    transf(List("a1","a2"), "c", subtr) ++
//    filter("c","res",is(6)) ++
//    reader("res",2)

  def connector =
      writer("a1",List(7)) ++
      writer("a2",List(4)) ++
      //    writer("a3",List(2)) ++
      //    writer("b",List(10)) ++
      //    nmerger(List("a1","a2","a3"),"a") ++
//      merger("a1","a2","a") ++
      transf(List("a1","a2"), "c", subtr) ++
      sync("c","res") ++
//      filter("c","res",is(6)) ++
      reader("res",2)


  var sol: Option[Solution] = connector.getConstraints.solveChocoDyn

  sol match {
    case Some(s) => println("Solved!\n"+s)
    case None => println("no solution")
  }

  sol = connector.getConstraints.solveXZ3

  sol match {
    case Some(s) => println("Z Solved!\n"+s)
    case None => println("Z no solution")
  }

  //  connector.step


  ////////////////////
  /// OLD Z3 way... //
  ////////////////////

//  println("\n----- Z3 INT constraints -----")
//
//  class Subtr extends reopp.common.IntFunction {
//    val z3Fun = (z3: Z3Context, ns: List[Z3AST]) =>
//      z3.mkSub(ns(0),ns(1))
//    val funFun = null
//    val choFun = null
//  }
//  class IsN(n: Int) extends reopp.common.IntPredicate {
//    val funPred = null
//    val choPred = null
//    val z3Pred = (z3: Z3Context, other: Z3AST) =>
//      z3.mkEq(other,z3.mkInt(n,z3.mkIntSort()))
//  }
//
//  def connectorZ3 =
//    writer("a1",List(2)) ++
//      writer("a2",List(4)) ++
//      writer("b",List(10)) ++
//      merger("a1","a2","a") ++
//      transf(List("b","a"), "c", new Subtr()) ++
//      filter("c","res",new IntPred(reopp.common.Utils.dataVar("c",0),new IsN(6))) ++
//      sdrain("c","res") ++
//      reader("res",2)
//
//
//  sol = connectorZ3.getConstraints.solvez3
//
//  sol match {
//    case Some(s) => println("Solved!\n"+s)
//    case None => println("no solution")
//  }
}
