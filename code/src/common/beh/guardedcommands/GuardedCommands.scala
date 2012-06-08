package common.beh.guardedcommands

import common.beh.Constraints
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt
import org.sat4j.specs.{TimeoutException, ContradictionException, ISolver}
import scala.collection.mutable.{Set => MutSet, Map => MutMap}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 18:14
 * To change this template use File | Settings | File Templates.
 */

class GuardedCommands extends Constraints[GCSolution,GuardedCommands] {

  var commands = Set[GuardedCom]()

  def toCNF: (CNF.Core,MutMap[String,Int]) = {
    var da = DomainAbst()
    for (c <- commands) da = da + c.da

    val afv = commands.map(_.afv(da)).foldRight[Set[String]](Set())(_++_)
    val fv = commands.map(_.fv).foldRight[Set[String]](Set())(_++_)
    var i = 1
    var vars = MutMap[String,Int]()
    for (v <- afv) {
      vars(v) = i
      i += 1
    }
//    // DEBUGGING
//    println("fv: "+fv.mkString(","))
//    println("afv: "+afv.mkString(","))
//    println("DA: "+da.pp)
//    for (v <- fv)
//      println("var "+v+" - domain: "+da.domain(v).mkString(","))

    val cnf = commands.map(_.toCNF(vars,da)).foldRight[CNF.Core](List())((x:CNF.Core,y:CNF.Core) => x ::: y)
    (cnf,vars)
  }

  type PEval = (Map[String,Int], Map[String,String])

  def partialEval(sol:GCSolution): (Map[String,Int], Map[String,Set[String]]) = {
    val pevals = commands.map(_.partialEval(sol)) // list of pairs of maps
//    val peval = pevals.foldRight[PEval]((Map(),Map()))((x:PEval,y:PEval) => (x._1++y._1, x._2++y._2))

    var vareqs = Map[String,Set[String]]().withDefaultValue(Set[String]())
    // invert mapping of assignments!
    for ((_,m) <- pevals; (v1,v2) <- m)
      vareqs += v2 -> (vareqs(v2) + v1)

    var initData = Map[String,Int]()
    for ((m,_) <- pevals)
      initData ++= m

    var newData = initData
    while (!newData.isEmpty) {
      val (s,d) = newData.head
      newData = newData.tail
      if (vareqs contains s)
        for (v2 <- vareqs(s)) {
          newData += v2 -> d
          initData += v2 -> d
        }
    }
    (initData,vareqs)
  }


  def solve: Option[GCSolution] = {

    val (cnf,vars) = toCNF
    val varname = MutMap[Int,String]()
    for ((k,v) <- vars)
      varname(v) = k

    val MAXVAR: Int = vars.size // cnf.nvars
    val NBCLAUSES: Int = cnf.size //nclauses
    // START TIMER BEFORE ADDNG CLAUSES TO THE MODEL!
    //val start: Long = System.currentTimeMillis();

    val solver: ISolver = SolverFactory.newDefault()

//    println("solving constraints:\n"+cnf.map(_.mkString(",")).mkString("[","\n ","]"))
//    println("vars:\n"+vars.mkString("\n"))


    solver.newVar(MAXVAR)
    solver.setExpectedNumberOfClauses(NBCLAUSES)
    try {
      for (cl <- cnf) {
        solver.addClause(new VecInt(cl))
      }

      // Solving stage
      val model = solver.findModel
  //    val stop: Long = System.currentTimeMillis()
  //    println("TimeMillis: " + (stop - start))

      if (model != null) {
        var res = Map[String,Boolean]()
        //println("Problem is satisfiable!!")
        for (i <- model) {
          if (i>0) res += varname(i)  -> true  //print(""+varname(i)+" ")
          else     res += varname(-i) -> false //print("~"+varname(-i)+" ")
        }
        Some(new GCSolution(res))
        //println("")
        //problem.findModel.mkString(" ")
      } else {
        None
        //println("Not satisfiable...")
      }
    } catch {
      case (e: ContradictionException) =>
        println("adding contraditory clause...")
        println("Not satisfiable...")
        None
      //	      e.printStackTrace()
      case (e: TimeoutException) =>
        System.out.println("Timed out....")
        e.printStackTrace()
        None
    }
  }

  def ++(other: GuardedCommands): GuardedCommands = {
    val thiscommands = commands
    new GuardedCommands(){
      commands = thiscommands ++ other.commands
    }
  }
  def ++(others: Iterable[GuardedCom]): GuardedCommands = {
    val thiscommands = commands
    new GuardedCommands(){
      commands = thiscommands ++ others
    }
  }
}


object GuardedCommands {
  def apply(gs: Set[GuardedCom]): GuardedCommands = {
    val g = new GuardedCommands()
    g.commands = gs
    g
  }
  def apply(g: GuardedCom): GuardedCommands = apply(Set(g))
  def apply(): GuardedCommands = new GuardedCommands()

}