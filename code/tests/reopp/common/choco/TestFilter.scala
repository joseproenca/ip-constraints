package reopp.common.choco

import reopp.common.{Utils}
import Utils.{flowVar,dataVar}
import org.scalatest.FunSpec
import dataconnectors.{ChoReader, ChoFilter, ChoWriter}
import ChoConnector.ChoBuilder._
import reopp.common.examples.{Even, GT}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 15:51
 * To change this template use File | Settings | File Templates.
 */

class TestFilter extends FunSpec {

  // NOTE: Choco has no longer FLOW AXIOM

  describe ("Choco - Writer, 2 Filters (>5, even), and a reader.") {

    val w: ChoConnector = new ChoWriter("w",42,List(7,5,6,7,8))
    val f1: ChoConnector = new ChoFilter("fi","fo",43,(new GT(5)).choPred)
    val f2: ChoConnector = new ChoFilter("fi","fo",44,(new Even).choPred)
    val rd: ChoConnector = new ChoReader("r",45,5)


    var c = w.getConstraints++ f1.getConstraints ++ f2.getConstraints ++ rd.getConstraints

//    w.connections += f1 -> Set(("w","fi",43))
//    f1.connections += f2 -> Set(("fo","fi",44))
//    f2.connections += rd -> Set(("fo","r",45))
//    c ++= ChoConstraints(
//      Var(flowVar("w",42)) or Var(flowVar("fi",43)) or
//      Var(flowVar("r",45)) or Var(flowVar("fi",44)) )
//    c = w.sync(f1,c)
//    c = f1.sync(f2,c)
//    c = f2.sync(rd,c)
////    c = c ++ Set(Var(ConstrBuilder.flowVar("x",44)))

    c ++=
      sync("fi",43,"w",42) ++
      sync("fi",44,"fo",43) ++
      sync("r",45,"fo",44)

    c.close()


    println(c.constrBuilders)

    val res = c.solve
    if (res.isDefined) println("solved:\n"+res.get)
    else println("no solution")

    it ("should have a solution")
    {assert(res.isDefined)}

    it ("after first filter there is data flow")
    {assert(res.get.hasFlowOn(flowVar("fo",43)))}

    it ("after first filter data is 7")
    {assert(res.get.getVal(dataVar("fo",43)) == Some(7))}

    it ("after 2nd filter there is no dataflow")
    {assert(!res.get.hasFlowOn(flowVar("fo",44)))}

  }
}
