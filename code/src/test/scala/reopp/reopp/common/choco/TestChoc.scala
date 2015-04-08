package reopp.common.choco

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 07/05/12
 * Time: 12:16
 * To change this template use File | Settings | File Templates.
 */

//import reopp.common.Utils
import org.scalatest.FunSpec
import choco.Choco
import choco.cp.solver.CPSolver
import choco.cp.model.CPModel
import choco.kernel.model.variables.integer.IntegerVariable
import connectors.{ChoLossy, ChoWriter}


//DEPRECATED - ChoConnector will fail

//class TestChoc extends FunSpec {
//
//  describe ("Choco - manual constraints") {
//    val x = Choco.makeBooleanVar("x")
//    val y = Choco.makeBooleanVar("y")
//    val c = Choco.and(Choco.eq(x,1),Choco.eq(y,0))
//
//    val s = new CPSolver
//
//    val m: CPModel = new CPModel
//
//    m.addConstraint(c)
//
//    println(m.pretty())
//
//    s.read(m);
//
//    val solved = s.solve
//    //  val solved = if (result == null)  false  else result
//
//    if (solved) {
//      var res: String = ""
//      val it: java.util.Iterator[IntegerVariable] = s.getModel.getIntVarIterator
//      while (it.hasNext) {
//        val variab: IntegerVariable = it.next
//        res += variab.getName + " -> " + s.getVar(variab).getVal + "\n";
//      }
//      print(res)
//    }
//    else  println("no solution")
//    println("---------------")
//
//    it ("should have solution") {assert(solved)}
//
//  }
//
//  describe ("Choco - Writer to a lossy") {
//
//    val s1: ChoConnector = new ChoWriter("a",42,2)
//    val s2: ChoConnector = new ChoLossy("b","c",43)
//
//    val c = s1.getConstraints++ s2.getConstraints
//    //s1.connections += this -> Set(("a","b",43))
//    val c2: ChoConstraints = c ++ ChoConnector.ChoBuilder.sync("b43","a42") //s1.sync(this,c)
//    c2.close()
//
//    println(c2)
//
//    val r = c2.solve() //c2.minimise(dataVar("a",42))
//    if (r.isDefined) {
//      println(r.get.choSol.getModel.pretty())
//      println("solved:\n"+r.get)
//      println("r.size (known vars): "+r.get.size)
//      println("r.sizeModel (vars in the solution): "+r.get.sizeModel)
//    }
//    else println("no solution")
//
//    it ("should have a solution")
//    {assert(r.isDefined)}
//
//    if (r.isDefined) {
//      it ("should have 2 variables in the solution")
//      {assert(r.get.sizeModel == 2)}
//
//      it ("should have 5 known variables")
//      {assert(r.get.size == 5)}
//    }
//
//  }
//}
