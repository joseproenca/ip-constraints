package reopp.common.guardedcommands.chocox

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 06/02/13.
 */

import org.scalatest.FunSpec
import reopp.common.guardedcommands.dataconnectors.{GCADrain, GCWriter, GCMerger, GCFilter}
import reopp.common.{Buffer, Utils, Function, Predicate}
import reopp.common.Utils._
import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import reopp.common.guardedcommands.{Impl, Formula}

class TestChocoX extends FunSpec {

  describe ("Simple connector") {


    val func = Function("func") {
      case x:String =>
        println("~~~~ asked func applied to "+x)
        "f-"+x
    }

    val pred = Predicate("pred") {
      case x:String =>
        println("~~~~ testing if f-aaa == "+x)
        x == ("f-aaa")
    }

    val c2 =
      writer("a",List("aaa")) ++
      transf("a","b",func) ++
      filter("b","c",pred) ++
      reader("c",2)



    val cs = c2.getConstraints
    cs.close()

    ////
    val a = mkVar("a")
    val b = mkVar("b")
    val c = mkVar("c")
    val cs2 = Formula(
      a := "aaa",
      (a :< pred) --> (b := a)
    )



    val res = ChocoX.solve(cs)

    println("-----------\n"+cs.commands.mkString("\n"))
    println("-----------")


    if (res.isDefined) print("solved:\n"+res.get)
    else println("no solution")

//    if (res.isDefined) println("partial eval: "+c.partialEval(res.get))

//    it ("c should have a sol") {assert (res.isDefined)}

  }

}
