package common.beh.choco

import dataconnectors._
import common.beh.Utils
import Utils.{flowVar,dataVar}
import org.scalatest.FunSpec

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 15:51
 * To change this template use File | Settings | File Templates.
 */

class TestFilter extends FunSpec {

  describe ("Choco - Writer, 2 Filters (>5, even), and a reader.") {

    val w: ChoBehaviour = new ChoWriter("w",42,List(7,5,6,7,8))
    val f1: ChoBehaviour = new ChoFilter("fi","fo",43,(new GT(5)).choPred)
    val f2: ChoBehaviour = new ChoFilter("fi","fo",44,(new Even).choPred)
    val rd: ChoBehaviour = new ChoReader("r",45,5)


    var c = w.constraints ++ f1.constraints ++ f2.constraints ++ rd.constraints
    w.connections += f1 -> Set(("w","fi",43))
    f1.connections += f2 -> Set(("fo","fi",44))
    f2.connections += rd -> Set(("fo","r",45))
    c = w.sync(f1,c)
    c = f1.sync(f2,c)
    c = f2.sync(rd,c)
//    c = c ++ Set(Var(ConstrBuilder.flowVar("x",44)))

    println(c.constrBuilders)

    val res = c.solve
    if (res.isDefined) println("solved:\n"+res.get.pretty)
    else println("no solution")

    it ("should have a solution")
    {assert(res.isDefined)}

    it ("after first filter there is data flow")
    {assert(res.get.hasFlow(flowVar("fo",43)))}

    it ("after first filter data is 7")
    {assert(res.get.getVal(dataVar("fo",43)) == Some(7))}

    it ("after 2nd filter there is no dataflow")
    {assert(!res.get.hasFlow(flowVar("fo",44)))}

  }
}
