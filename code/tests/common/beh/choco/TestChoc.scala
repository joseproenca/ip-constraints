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
import choco.Choco
import choco.cp.solver.CPSolver
import choco.cp.model.CPModel
import choco.kernel.model.variables.integer.IntegerVariable


class TestChoc extends FunSpec {

  describe ("Choco - manual constraints") {
    val x = Choco.makeBooleanVar("x")
    val y = Choco.makeBooleanVar("y")
    val c = Choco.and(Choco.eq(x,1),Choco.eq(y,0))

    val s = new CPSolver

    val m: CPModel = new CPModel

    m.addConstraint(c)

    println(m.pretty())

    s.read(m);

    val solved = s.solve
    //  val solved = if (result == null)  false  else result

    if (solved) {
      var res: String = ""
      val it: java.util.Iterator[IntegerVariable] = s.getModel.getIntVarIterator
      while (it.hasNext) {
        val variab: IntegerVariable = it.next
        res += variab.getName + " -> " + s.getVar(variab).getVal + "\n";
      }
      print(res)
    }
    else  println("no solution")
    println("---------------")

    it ("should have solution") {assert(solved)}

  }

  describe ("Choco - Writer to a lossy") {

    val s1: ChoBehaviour = new ChoWriter("a",42,2)
    val s2: ChoBehaviour = new ChoLossy("b","c",43)


    val c = s1.constraints ++ s2.constraints
    s1.connections += this -> Set(("a","b",43))
    val c2 = s1.sync(this,c)

    val r = c2.solve
    if (r.isDefined) {
      println(r.get.choSol.getModel.pretty())
      println("solved:\n"+r.get.pretty)
    }
    else println("no solution")

    it ("should have a solution")
    {assert(r.isDefined)}

    if (r.isDefined) {
      it ("should have 2 variables in the solution")
      {assert(r.get.sizeModel == 2)}

      it ("should have 3 known variables")
      {assert(r.get.size == 3)}
    }

  }
}
