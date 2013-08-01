package reopp.common.choco

import org.scalatest.FunSpec
import reopp.common.choco.ChoConnector.ChoBuilder._
import reopp.common.choco.connectors.{ChoReader, ChoMerger, ChoWriter}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 18:23
 * To change this template use File | Settings | File Templates.
 */

class TestMerger extends FunSpec {

  describe ("Choco - 2Writers, merger, and a reader") {

    val w1: ChoConnector = new ChoWriter("w",42,1)
    val w2: ChoConnector = new ChoWriter("w",43,1)
    val m: ChoConnector = new ChoMerger("x","y","a",44)
    val r2: ChoConnector = new ChoReader("r",45,2)


    var c = w1.getConstraints ++ w2.getConstraints ++ m.getConstraints ++ r2.getConstraints
//    w1.connections += m -> Set(("w","x",44))
//    w2.connections += m -> Set(("w","y",44))
//    m.connections += r2 -> Set(("a","r",45))
//    c = w1.sync(m,c)
//    c = w2.sync(m,c)
//    c = m.sync(r2,c)
//    c = c ++ Set(Var(Utils.flowVar("x",44)))

    c ++=
      sync("x",44,"w",42) ++
      sync("y",44,"w",43) ++
      sync("r",45,"a",44)

    c.close()


    println(c.constrBuilders)

    val r = c.solve
    if (r.isDefined) println("solved:\n"+r.get)
    else println("no solution")

    it ("should have a solution")
    {assert(r.isDefined)}

    it ("should have 3 variables in the solution")
    {assert(r.get.sizeModel == 3)}

    it ("should have 12 known variables")
    {assert(r.get.size == 12)}

  }
}
