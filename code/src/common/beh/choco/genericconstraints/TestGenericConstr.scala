package common.beh.choco.genericconstraints

import choco.kernel.model.Model
import choco.cp.model.CPModel
import choco.kernel.model.variables.integer.IntegerVariable
import choco.Choco
import choco.kernel.solver.Solver
import choco.cp.solver.CPSolver
import java.util
import common.beh.{Predicate, Function}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 14:25
 * To change this template use File | Settings | File Templates.
 */

object TestGenericConstr extends App {

  // Creation of the model
  val m: Model = new CPModel()
  // Declaration of the variable
  val aVar: IntegerVariable = Choco.makeIntVar("a_var", 0, 4)
  val bVar: IntegerVariable = Choco.makeIntVar("b_var", 0, 4)
  val cVar: IntegerVariable = Choco.makeIntVar("c_var", 0, 4)
  val dVar: IntegerVariable = Choco.makeIntVar("d_var", 0, 1)

  val xpred: IntegerVariable = Choco.makeBooleanVar("xpred")
  val xflow: IntegerVariable = Choco.makeBooleanVar("xflow")
  val yflow: IntegerVariable = Choco.makeBooleanVar("yflow")

  val gt5 = new Predicate {
    def check(x: Any) = x.asInstanceOf[Int] > 5
    override def toString = "[>5]"
  }
  val twicePlusOne = new util.ArrayList[Function]
  twicePlusOne.add(new Function {
    def calculate(x: Any) = x.asInstanceOf[Int] * 2
    override def toString = "[*2]"
  })
  twicePlusOne.add(new Function {
    def calculate(x: Any) = x.asInstanceOf[Int] + 1
    override def toString = "[+1]"
  })


  m.addVariable(xflow)
  val cc = PredManager.genConstr(xpred,xflow,yflow,2,new Buffer, gt5, twicePlusOne)

//    m.addConstraint(Choco.allDifferent(dVar,xflow))
//    m.addConstraint(Choco.eq(dVar,0))
    m.addConstraint(Choco.eq(xflow,1))
    m.addConstraint(Choco.eq(xpred,0))
//    m.addConstraint(Choco.not(cc))
//    m.addConstraint(Choco.or(Choco.eq(xpred,0),cc))
    m.addConstraint(Choco.implies(Choco.TRUE,Choco.and(Choco.TRUE,cc)))
//    m.addConstraint(Choco.reifiedConstraint(dVar,cc))

  println(m.pretty())

  val s: Solver = new CPSolver()

  s.read(m)
  val solved = s.solve()

  var res: String = "solved? "+solved+"\n"
  if (!solved) res += "--- NO SOL ---\n"
  val it: java.util.Iterator[IntegerVariable] = s.getModel.getIntVarIterator
  while (it.hasNext) {
    val variab: IntegerVariable = it.next
    res += variab.getName + " -> " + s.getVar(variab).getVal + "\n"
  }
  res += "-\n"
  // ERROR if varMap has has IntegerVariables that are not in the solution....

  println(res)

}
