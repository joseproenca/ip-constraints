package common.examples

import common.guardedcommands.dataconnectors.ConnectorGen._
import common.Predicate

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

  val sol = connector.getConstraints.solveChocoDyn

  sol match {
    case Some(s) => println("Solved!\n"+s)
    case None => println("no solution")
  }

  connector.step
}
