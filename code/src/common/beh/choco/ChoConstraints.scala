package common.beh.choco

import choco.kernel.model.constraints.Constraint
import choco.cp.solver.CPSolver
import choco.kernel.model.variables.integer.IntegerVariable
import choco.cp.model.CPModel
import choco.kernel.common.logging.{Verbosity, ChocoLogging}
import common.beh.Constraints

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:22
 * To change this template use File | Settings | File Templates.
 */

class ChoConstraints extends common.beh.Constraints[ChoSolution,ChoConstraints] {
  type mytype = ChoConstraints

//  var constraints = List[Constraint]()         // kept in sync -,
  var constrBuilders = List[ConstrBuilder]()   // kept in sync -'
//  var varMap: Map[String, IntegerVariable] = Map[String, IntegerVariable]()

//  def impose(c:Constraint) = constraints ::= c
  def impose(c:ConstrBuilder) {
    constrBuilders ::= c

//    val choco = c.toChoco(varMap)
//    varMap = choco._1
//    constraints ::= choco._2
  } 

  def solve: Option[ChoSolution] = {
    ChocoLogging.setVerbosity(Verbosity.OFF)

    val s = new CPSolver
    // lazy builder
//    var varMap = Map[String, IntegerVariable]()

    val m: CPModel = new CPModel

    val pair = ConstrBuilder.toChoco(constrBuilders)
    val varMap = pair._1
    for (constr <- pair._2)
      m.addConstraint(constr)

    println(m.pretty())

    s.read(m);

    val solved = s.solve
    //  val solved = if (result == null)  false  else result
    
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
//    cs.constraints = constraints
    cs.constrBuilders = constrBuilders
//    cs.varMap = varMap
    cs ++= other
    cs
  }
  def ++ (other:Iterable[ConstrBuilder]): ChoConstraints = {
    val cs = new ChoConstraints
//    cs.constraints = constraints
    cs.constrBuilders = constrBuilders
//    cs.varMap = varMap
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
//  def apply(cs: List[Constraint]): ChoConstraints = {
//    val res = new ChoConstraints
//    res.constraints = cs
//    res
//  }
}