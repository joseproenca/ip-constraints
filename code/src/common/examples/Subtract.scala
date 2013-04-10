package common.examples

import common.guardedcommands.dataconnectors.ConnectorGen._
import common.{Solution, Predicate}
import z3.scala.{Z3AST, Z3Context}
import common.guardedcommands.IntPred
import common.Utils._
import common.guardedcommands.IntPred
import scala.Some

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 10/04/13.
 */
object Subtract extends App {

  val subtr = common.Function("subtract"){
    case List(n1:Int,n2:Int) => n1 - n2
    case x => sys.error("wrong arguments for subtract "+x+":"+x.getClass)
  }

  def is(n:Int) = Predicate("(=="+n+")") {
    case x: Int => x == n
  }

  val connector =
    writer("a1",List(2)) ++
    writer("a2",List(4)) ++
    writer("b",List(10)) ++
    merger("a1","a2","a") ++
    transf(List("b","a"), "c", subtr) ++
    sfilter("c","res",is(6)) ++
    reader("res",2)


  var sol: Option[Solution] = connector.getConstraints.solveChocoDyn

  sol match {
    case Some(s) => println("Solved!\n"+s)
    case None => println("no solution")
  }

//  connector.step


  ////////////////////
  /// OLD Z3 way... //
  ////////////////////

  println("\n----- Z3 INT constraints -----")

  class Subtr extends common.IntFunction {
    val z3Fun = (z3: Z3Context, ns: List[Z3AST]) =>
      z3.mkSub(ns(0),ns(1))
    val funFun = null
    val choFun = null
  }
  class IsN(n: Int) extends common.IntPredicate {
    val funPred = null
    val choPred = null
    val z3Pred = (z3: Z3Context, other: Z3AST) =>
      z3.mkEq(other,z3.mkInt(n,z3.mkIntSort()))
  }

  val connectorZ3 =
    writer("a1",List(2)) ++
      writer("a2",List(4)) ++
      writer("b",List(10)) ++
      merger("a1","a2","a") ++
      transf(List("b","a"), "c", new Subtr()) ++
      filter("c","res",new IntPred(common.Utils.dataVar("c",0),new IsN(6))) ++
      sdrain("c","res") ++
      reader("res",2)


  sol = connectorZ3.getConstraints.solvez3

  sol match {
    case Some(s) => println("Solved!\n"+s)
    case None => println("no solution")
  }

}
