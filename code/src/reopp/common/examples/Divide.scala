package reopp.common.examples

import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import reopp.common.{NoneSol, SomeSol}

/**
 * Divide 100 by the input value. Shows the usage of partial functions.
 * Note that several things are partial functions in Scala, including most scala sequences (lists) and maps.
 *
 * Created by jose on 12/04/13.
 */
object Divide extends App {

  val seed = 100

  // type cannot be inferred for this (and most) partial functions. Needed type of the argument.
  val divide: PartialFunction[Int,Int] =
    { case d: Int if d != 0 => seed / d }

  //def div2()

  val connector =
    writer("a",List(0)) ++
    writer("b",List(2)) ++
    transf("a","aout",divide) ++
    transf("b","bout",divide) ++
    sdrain("a","b") ++
    reader("aout",1) ++
    reader("bout",1)

  val sol = connector.getConstraints.solveChocoDyn

  sol match {
    case SomeSol(s) => println("solved!\n"+s)
    case _          => println("no sol")
  }

}
