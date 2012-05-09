package common.beh.choco

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 07/05/12
 * Time: 12:16
 * To change this template use File | Settings | File Templates.
 */

import connectors.{ChoWriter, ChoLossy}
import org.scalatest.FunSpec


class TestChoc extends FunSpec {

  describe ("CHoco - Writer to a lossy") {

    val s1: ChoBehaviour = new ChoWriter("a",42,2)
    val s2: ChoBehaviour = new ChoLossy("b","c",43)


    val c = s1.constraints ++ s2.constraints
    s1.connections += this -> Set(("a","b",43))
    val c2 = s1.sync(this,c)

    val r = c2.solve
    if (r.isDefined) println("solved:\n"+r.get.pretty)
    else println("no solution")

    it ("should have a solution")
    {assert(r.isDefined)}

    it ("should have 2 variables in the solution")
    {assert(r.get.sizeModel == 2)}

    it ("should have 3 known variables")
    {assert(r.get.size == 3)}

  }
}
