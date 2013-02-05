package common.choco

import choco.cp.solver.CPSolver
import choco.cp.model.CPModel
import choco.kernel.common.logging.{Verbosity, ChocoLogging}
import genericconstraints.Buffer
import scala.collection.JavaConversions._
import choco.Choco
import choco.kernel.model.variables.integer.IntegerVariable
import common.{Utils, Constraints}
import Utils._
import choco.cp.solver.search.integer.branching.AssignVar
import choco.cp.solver.search.integer.varselector.StaticVarOrder
import choco.kernel.solver.variables.integer.IntDomainVar
import choco.cp.solver.search.integer.valiterator.IncreasingDomain
import common.Constraints

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:22
 * To change this template use File | Settings | File Templates.
 */

class ChoConstraints extends Constraints[ChoSolution,ChoConstraints] {
  type mytype = ChoConstraints

  var constrBuilders = List[ConstrBuilder]()   // kept in sync -'

  def impose(c:ConstrBuilder) {
    constrBuilders ::= c
  }

  def minimise(v: String): Option[ChoSolution] = {
    ChocoLogging.setVerbosity(Verbosity.OFF)

    val s = new CPSolver

    val m: CPModel = new CPModel

    val pair = ConstrBuilder.toChoco(constrBuilders,new Buffer)
    val varMap = pair._1
    for (constr <- pair._2)
      m.addConstraint(constr)

    // Add flow constraints
    var flowvars = Set[IntegerVariable]()
    for (x <- m.getIntVarIterator) {
      if (isFlowVar(x.getName)) flowvars += x
    }
    if (!(flowvars.isEmpty)) {
      var c = Choco.eq(flowvars.head,1)
      for (v <- flowvars.tail)
        c = Choco.or(Choco.eq(v,1),c)
      m.addConstraint(c)
    }

    //    println(m.pretty())

    s.read(m)


//    for ((k,v) <- varMap)
//      res += k + " -> " + choSol.getVar(v).getVal + "\n"

    var solved = false
    if (varMap contains v)
      solved = s.minimize(s.getVar(varMap(v)),true)
    else println("Var not found in minimisation. VarMap: "+varMap.mkString(","))

    if (solved) Some(new ChoSolution(s,varMap))
    else  None

  }


  /**
   * Add flow constraints and CC3 constraints.
   * Not in use yet.
   */
  def close() {
    // Add flow constraints
    var flowvars = Set[String]()
    for (cb <- constrBuilders; x <- cb.getVars) {
      if (isFlowVar(x)) flowvars += x
    }
    if (!(flowvars.isEmpty)) {
      var c: ConstrBuilder = Var(flowvars.head)
      for (v <- flowvars.tail)
        c = Or(Var(v),c)
      impose(c)
    }

  }

  def solve: Option[ChoSolution] = solve(List(), new Buffer)

  def solve(buf: Buffer): Option[ChoSolution] = solve(List(),buf)

  def solve(order:List[String], buf: Buffer): Option[ChoSolution] = {
    ChocoLogging.setVerbosity(Verbosity.OFF)

    val s = new CPSolver

    val m: CPModel = new CPModel

    val pair = ConstrBuilder.toChoco(constrBuilders, buf)
    val varMap = pair._1
    for (constr <- pair._2)
      m.addConstraint(constr)

//    // Add flow constraints
//    var flowvars = Set[IntegerVariable]()
//    for (x <- m.getIntVarIterator) {
//      if (isFlowVar(x.getName)) flowvars += x
//    }
//    if (!(flowvars.isEmpty)) {
//      var c = Choco.eq(flowvars.head,1)
//      for (v <- flowvars.tail)
//        c = Choco.or(Choco.eq(v,1),c)
//      m.addConstraint(c)
//    }

//    println(m.pretty())

    s.read(m)

    // If there is an order of variables passed, use a strategy based on that order.
    if (!(order isEmpty))
      s.addGoal(new AssignVar(new StaticVarOrder(s,buildOrder(order, s)),new IncreasingDomain))

    val solved = s.solve

    if (solved) Some(new ChoSolution(s,varMap))
    else  None
  }


  private def buildOrder(order: List[String], s: CPSolver): Array[IntDomainVar] = {
    var flowvar = List[IntDomainVar]()
    //      var datavar = List[IntDomainVar]()
    val svars = scala.collection.mutable.Map[String, IntDomainVar]()

    for (v <- s.getIntVarIterator)
      if (!(v.getName startsWith "TMP_")) svars(v.getName) = v   // add non-tmp vars to svars

//    println("var names in the solver: "+svars.keys.mkString(" - "))
//    println("vars in the order: "+order.mkString(" -> "))
//    println("port names in the solver: "+svars.keys.map(var2port(_)).mkString(" - "))
//    println("port vars in the order: "+order.map(var2port(_)).mkString(" -> "))


    for (v <- order) {                    // for the next varname
      if (svars contains data2flow((v)))  // checks if it has a real (flow) variable
        flowvar ::= svars(data2flow(v))   // append it to flowvar
      //        if (svars contains v)               datavar ::= svars(v)
      svars -= v
      svars -= data2flow(v)
    }

    val fullorder: Array[IntDomainVar] = (flowvar ::: svars.values.toList).toArray
    //      val fullorder: Array[IntDomainVar] = (flowvar.reverse ::: svars.values.toList.sortBy(_.getName()).reverse).toArray
    //      val fullorder: Array[IntDomainVar] = (svars.values.toList/*.sortBy(_.getName())*/ ::: datavar::: flowvar).toArray


//////// Attempt to group all variables to the same port, and follow the order.
//    var domVars = List[IntDomainVar]()
//    val vars = scala.collection.mutable.Map[String, Set[IntDomainVar]]().withDefaultValue(Set[IntDomainVar]())
//
//    for (v <- s.getIntVarIterator)
//      if (!(v.getName startsWith "TMP_")) {
//        val port = var2port(v.getName)
//        vars(port) = vars(port) + v
//      }
//
//    for (v <- order) {
//    val port = var2port(v)
//      if (vars contains port)
//        domVars :::= vars(port).toList
//    }



//    println("--- old new order: " + fullorder.mkString(","))
//    println("--- new new order: " + domVars.mkString(","))

    fullorder
//    domVars.reverse.toArray
  }

  def impose(cs:ChoConstraints) {
    for (cb <- cs.constrBuilders)
      impose(cb) // need to avoid creating different constraint variables for variables with the same name
  }
  def impose(cs:Iterable[ConstrBuilder]) {
    for (cb <- cs)
      impose(cb) // need to avoid creating different constraint variables for variables with the same name
  }

  def ++ (other:ChoConstraints): ChoConstraints = {
    val cs = new ChoConstraints
    cs.constrBuilders = constrBuilders
    cs impose other
    cs
  }
  def ++ (other:Iterable[ConstrBuilder]): ChoConstraints = {
    val cs = new ChoConstraints
    cs.constrBuilders = constrBuilders
    cs impose other
    cs
  }

  override def toString = {
    constrBuilders.mkString("[",", ","]")
  }
}
  
  

object ChoConstraints {
  def apply(c: ConstrBuilder): ChoConstraints = {
    val res = new ChoConstraints
    res impose c
    res
  }
  def apply(cs: Iterable[ConstrBuilder]): ChoConstraints = {
    val res = new ChoConstraints
    for (c <- cs) res impose c
    res
  }
  //  def apply(cs: List[Constraint]): ChoConstraints = {
//    val res = new ChoConstraints
//    res.constraints = cs
//    res
//  }
}