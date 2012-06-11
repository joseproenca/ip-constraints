package common.beh.choco

import choco.cp.solver.CPSolver
import choco.cp.model.CPModel
import choco.kernel.common.logging.{Verbosity, ChocoLogging}
import scala.collection.JavaConversions._
import choco.Choco
import choco.kernel.model.variables.integer.IntegerVariable
import common.beh.Utils

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:22
 * To change this template use File | Settings | File Templates.
 */

class ChoConstraints extends common.beh.Constraints[ChoSolution,ChoConstraints] {
  type mytype = ChoConstraints

  var constrBuilders = List[ConstrBuilder]()   // kept in sync -'

  def impose(c:ConstrBuilder) {
    constrBuilders ::= c
  }

  def solve: Option[ChoSolution] = {
    ChocoLogging.setVerbosity(Verbosity.OFF)

    val s = new CPSolver

    val m: CPModel = new CPModel

    val pair = ConstrBuilder.toChoco(constrBuilders)
    val varMap = pair._1
    for (constr <- pair._2)
      m.addConstraint(constr)

    // Add flow constraints
    var flowvars = Set[IntegerVariable]()
    for (x <- m.getIntVarIterator) {
      if (Utils.isFlowVar(x.getName)) flowvars += x
    }
    if (!(flowvars.isEmpty)) {
      var c = Choco.eq(flowvars.head,1)
      for (v <- flowvars.tail)
        c = Choco.or(Choco.eq(v,1),c)
      m.addConstraint(c)
    }

//    println(m.pretty())

    s.read(m)

    val solved = s.solve

    if (solved) Some(new ChoSolution(s,varMap))
    else  None
  }

  def ++= (cs:ChoConstraints) {
    for (cb <- cs.constrBuilders)
      impose(cb) // need to avoid creating different constraint variables for variables with the same name
  }
  def ++= (cs:Iterable[ConstrBuilder]) {
    for (cb <- cs)
      impose(cb) // need to avoid creating different constraint variables for variables with the same name
  }

  def ++ (other:ChoConstraints): ChoConstraints = {
    val cs = new ChoConstraints
    cs.constrBuilders = constrBuilders
    cs ++= other
    cs
  }
  def ++ (other:Iterable[ConstrBuilder]): ChoConstraints = {
    val cs = new ChoConstraints
    cs.constrBuilders = constrBuilders
    cs ++= other
    cs
  }
}
  
  

object ChoConstraints {
  def apply(c: ConstrBuilder): ChoConstraints = {
    val res = new ChoConstraints
    res.impose(c)
    res
  }
  def apply(cs: List[ConstrBuilder]): ChoConstraints = {
    val res = new ChoConstraints
    for (c <- cs) res.impose(c)
    res
  }
  //  def apply(cs: List[Constraint]): ChoConstraints = {
//    val res = new ChoConstraints
//    res.constraints = cs
//    res
//  }
}