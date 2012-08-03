package common.beh.guardedcommands

import common.beh.Constraints
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt
import org.sat4j.specs.{TimeoutException, ContradictionException, ISolver}
import scala.collection.mutable.{Set => MutSet, Map => MutMap}
import common.beh.Utils._
import common.beh.choco._
import genericconstraints.Buffer
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 18:14
 * To change this template use File | Settings | File Templates.
 */

class GuardedCommands extends Constraints[GCSolution,GuardedCommands] {

  var log = System.out
//  val log = new java.io.PrintStream(new java.io.FileOutputStream("/dev/null"))
  var justInit = false

  var commands = Set[GuardedCom]()
  var da = DomainAbst()
  var solvedDomain = false
  //  var someVars: Option[MutMap[String,Int]] = None
  val buf = new Buffer


  /**
   * Collect the domain of every guarded command in field 'da'.
   * Used by 'collectVars'.
   */
  def solveDomain() {
    if (!solvedDomain) {
      for (c <- commands) da = da + c.da
      println(da.pp)
      solvedDomain = true
    }
  }

  /**
   * Combines the free variables of all guarded commands
   * @return free variables of all guarded commands
   */
  def fv() = commands.map(_.fv).foldRight[Set[String]](Set())(_++_)

  /**
   * Should be performed only once per guarded command.
   * Possible optimization: collect vars upon creation
   *   - ignored since lots of temporary domains are calculated.
   * @return Map from var names to their unique id number from 1 to size(map) - to be used by a SAT solver.
   */
  def collectVars: MutMap[String, Int] = {
//    if (someVars.isDefined) return someVars.get

    solveDomain()

//    println(da.pp)

    val afv = commands.map(_.afv(da)).foldRight[Set[String]](Set())(_ ++ _)
    var i = 1
    var vars = MutMap[String, Int]()
    for (v <- afv) {
      println(" ** "+v)
      vars(v) = i
      i += 1
    }

    // TODO: optimize constraints: search for eq vars and assign the same var (int)!

//    someVars = Some(vars)
    vars
  }


  /**
   * Collects the SAT problem for the abstract problem. Uses 'collectVars' (and indirectly 'solveDomain').
   * @return  The CNF problem for the abstract problem.
   */
  def toCNF: (CNF.Core,MutMap[String,Int]) = {
    var vars: MutMap[String, Int] = collectVars

//    // DEBUGGING
//    val fv = commands.map(_.fv).foldRight[Set[String]](Set())(_++_)
//    println("fv: "+fv.mkString(","))
//    println("afv: "+afv.mkString(","))
//    println("DA: "+da.pp)
//    for (v <- fv)
//      if (!da.domain(v).isEmpty)
//        println("var "+v+" - domain: "+da.domain(v).mkString(","))


    val cnf = commands.map(_.toCNF(vars,da)).foldRight[CNF.Core](List())((x:CNF.Core,y:CNF.Core) => x ::: y)
    (cnf,vars)
  }


  /**
   * Calculates a partial evaluation based on a solution to its abstract problem.
   * @param sol Solution to this' abstract problem
   * @return A partial evaluation - initially only a set of (var and data) assignments and function applications.
   */
  def partialEval(sol:GCBoolSolution): PEval = { //(Map[String,Int], Map[String,Set[String]]) = {
    val pevals = commands.map(_.partialEval(sol)) // list of pairs of maps
    val peval = pevals.foldRight[PEval](new PEval(Map(),Map(),Map()))((x:PEval,y:PEval) => x++y)//(x._1++y._1, x._2++y._2))
    peval
  }


  /**
   * For each group of equal variables (by v1 := v2) that had no data assignment,
   * assumes that it has no solution (and hence the abstract solution was wrong).
   * This method uses the previous solution to increment the abstract problem, and
   * solves the new abstract problem.
   * Only groups that do not depend in any function are considered.
   * @param cnf The current abstract problem
   * @param vars The variable mapping between var names and their ids in the cnf
   * @param sol The previous solution - what should be negated to avoid being again a (wrong) solution.
   * @param pEval The current partial evaluation - with the equal groups that remain to be solved.
   * @return Triple (nested pair) with a solution for an extended abstract problem, this problem,
   *         and the mapping of var names to ids.
   * */
  def incrementAndSolve(cnf: CNF.Core,
                        vars: MutMap[String,Int],
                        sol: GCBoolSolution,
                        pEval: PEval) : (Option[GCBoolSolution],(CNF.Core,MutMap[String,Int])) = {
    // for each remaining group, negate it in the constraints
    var newCnf = cnf
    for (group <- pEval.flattenRest) {
      var avoid = List[Int]()
      // check if data is calculated by another group + function
      var funDependent = false
      for (modvars <- pEval.funcs.values; (v,_) <- modvars)
        if (group contains v) funDependent = true

      // collect bad solution for each remaining group with no functional dependency
      if (!funDependent) for (v <- group) {
        if (da.max contains v) {
          for ((pred,fs) <- da.domain(v)) {
            val pvar = predVar(v,pred,fs)
            avoid ::= (if (sol(pvar)) vars(pvar) * (-1)
            else vars(pvar))
          }
          newCnf ::= avoid.toArray
//          println("added new invariant: "+avoid.mkString(",")+" - "+vars)
          avoid = List()
        }
      }
    }
    (solveBool(newCnf,vars),(newCnf,vars))
  }



  def solve : Option[GCSolution] = {
    // solveDomain; toCNF; solveBool?; [partialEval; quotient; dataAssign(done?); solveData(done?); incrementAndSolve?]
    val time = System.currentTimeMillis()
    val cnf = toCNF
//    println("cnf: "+cnf)
//    println("GC: "+commands.mkString(","))
    val optSolBool = solveBool(cnf._1,cnf._2)
//    log.println("[SAT solve] "+(System.currentTimeMillis()-time))
    if (!optSolBool.isDefined)
      return None
//    println("INIT GUESS:\n"+optSolBool.get.pretty)

    if (justInit) {
      return Some(new GCSolution(optSolBool.get,Map[String, Int]()))
    }

    val res = loopPartialEval(partialEval(optSolBool.get),cnf,optSolBool.get,time)
//    log.println("[all solve] "+(System.currentTimeMillis()-time))
    res
  }

  //private var counter
  private def loopPartialEval(pEval: PEval,cnf: (CNF.Core,MutMap[String,Int]), solBool: GCBoolSolution,time: Long): Option[GCSolution] = {
//    println("#> solved  pEval             - "+pEval)
    pEval.quotient()
//    println("#> calculated quotient       - "+pEval)
//    log.println("[quotient] "+(System.currentTimeMillis()-time))
    pEval.applyDataAssgn(solBool)
//    println("#> solved simple data assign - "+pEval)
//    log.println("[apply data] "+(System.currentTimeMillis()-time))
    if (pEval.isFinished) return Some(pEval.getSol(solBool))
    pEval.solveSimpleData(solBool,da)
//    println("#> solved domain elements    - "+pEval)
    if (pEval.isFinished) return Some(pEval.getSol(solBool))
    val (optSol2,newcnf) = incrementAndSolve(cnf._1,cnf._2,solBool,pEval)
//    println("#> incremented and solved new constr.")//+newcnf)
    if (!optSol2.isDefined) return None

//    println("#> restarting loop...") //        - "+optSol2.get.pretty)
    loopPartialEval(partialEval(optSol2.get),newcnf,optSol2.get,time)
  }

  def solveBool: Option[GCBoolSolution] = {
    val (cnf,vars) = toCNF
    solveBool(cnf,vars)
  }

  def solveBool(c: CNF.Core, vars: MutMap[String,Int]): Option[GCBoolSolution] = {

    // store variable names and ADD FLOW CONSTRAINTS!
    val varname = MutMap[Int,String]()
    var flowvars = Set[Int]()
    for ((k,v) <- vars) {
      varname(v) = k
      if (isFlowVar(k)) flowvars += v
    }
    val cnf = flowvars.toArray :: c


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
        Some(new GCBoolSolution(res))
        //println("")
        //problem.findModel.mkString(" ")
      } else {
        None
        //println("Not satisfiable...")
      }
    } catch {
      case (e: ContradictionException) =>
//        println("adding contraditory clause...")
//        println("Not satisfiable...")
        None
      //	      e.printStackTrace()
      case (e: TimeoutException) =>
        System.out.println("Timed out....")
        e.printStackTrace()
        None
    }
  }


  /////////////////////////////////
  // USING DETERMINED CONNECTORS //
  /////////////////////////////////

  /**
   *  Returns a solution for determined and closed connectors
   * @return Data solution from the abstract predicates with a simple data propagation traversal, if the simple
   *         propagation returns a complete solution.
   */
  def quickDataSolve : Option[GCSolution] = {
    val cnf = toCNF
    val optSolBool = solveBool(cnf._1,cnf._2)
    if (!optSolBool.isDefined)
      return None

    val pEval = partialEval(optSolBool.get)
    val done = pEval.freshTraversal(buf)

    if (done)
      Some(pEval.getSol(optSolBool.get))
    else {
      println("failed... peval:\n"+pEval.toString)
      println("failed... boolSol:\n"+optSolBool.get.pretty)
      None
    }
  }


  /**
   *  Returns a solution for determined and closed connectors using Lazy constraints in Choco and smart variable ordering.
   * @return Data solution from the abstract predicates with a simple data propagation traversal, if the simple
   *         propagation returns a complete solution.
   */
  def lazyDataSolve : Option[GCSolution] = {
    val builders = toBoolConstrBuilders
//    val buf = new Buffer // using the same to solve boolean constraints and to get a solution while traversing the tree.
    val optSolBool = solveChocoBool(builders)

    if (!optSolBool.isDefined)
      return None

    val pEval = partialEval(optSolBool.get)
    val done = pEval.freshTraversal(buf)

    if (done)
      Some(pEval.getSol(optSolBool.get))
    else
      None
  }

  /////////////////////////
  // USING CHOCO FOR SMT //
  /////////////////////////

  def solveChoco : Option[ChoSolution] = {
    val choConstr = ChoConstraints(toConstBuilders)
    choConstr.solve
  }

  def toConstBuilders:Iterable[ConstrBuilder] = {
    for (com <- commands)
      yield com.toConstrBuilder
  }



  /////////////////////////
  // USING CHOCO FOR SAT //
  /////////////////////////

  def solveChocoSat : Option[GCSolution] = {
    // solveDomain; toCNF; solveBool?; [partialEval; quotient; dataAssign(done?); solveData(done?); incrementAndSolve?]
    val time = System.currentTimeMillis()
//    val buf = new Buffer
    val builders = toBoolConstrBuilders
//    println("#> solving abst using choco SAT cnf - "+da.pp)
//    println("builder: "+builders.mkString("\n"))
    val optSolBool = solveChocoBool(builders)
//    log.println("[Cho/SAT solve] "+(System.currentTimeMillis()-time))
    if (!optSolBool.isDefined)
      return None
//    println("INIT GUESS:\n"+optSolBool.get.pretty)

    if (justInit) return Some(new GCSolution(optSolBool.get,Map[String, Int]()))

    val res = loopPartialEvalCho(partialEval(optSolBool.get),builders,optSolBool.get)
//    log.println("[all solve]     "+(System.currentTimeMillis()-time))
    res
  }

  private def loopPartialEvalCho(pEval: PEval,builders: Iterable[ConstrBuilder], solBool: GCBoolSolution)
      : Option[GCSolution] = {
//    println("#> solved  pEval             - "+pEval)
    pEval.quotient()
//    println("#> calculated quotient       - "+pEval)
    pEval.applyDataAssgn(solBool)
//    println("#> solved simple data assign - "+pEval)
    if (pEval.isFinished) return Some(pEval.getSol(solBool))
    pEval.solveSimpleData(solBool,da)
//    println("#> solved domain elements    - "+pEval)
    if (pEval.isFinished) return Some(pEval.getSol(solBool))
    val (optSol2,newbuilders) = incrementAndSolve(builders,solBool,pEval)
//    println("#> incremented and solved new constr.")//+newcnf)
    if (!optSol2.isDefined) return None

//    println("#> restarting loop...") //        - "+optSol2.get.pretty)
    loopPartialEvalCho(partialEval(optSol2.get),newbuilders,optSol2.get)
  }


  /**
   * Calculate the choco integer constraints for the abstract (SAT) problem.
   * @return A constraint builder for choco constraints.
   */
  def toBoolConstrBuilders : Iterable[ConstrBuilder] = {
    solveDomain()

//    println("&& domain solved: "+da.pp)

    for (com <- commands)
      yield com.toBoolConstrBuilder(da)
  }



  def solveChocoBool : Option[GCBoolSolution] = {
    val builders = toBoolConstrBuilders
    //    println("#> solving abst using choco SAT cnf - "+da.pp)
//    println("builder: "+builders.mkString("\n"))
    solveChocoBool(builders)
  }


    /**
     * Same as "solveBool" but using choco constraints (from constraint builders).
     * Optimised solver: lazy constraints and variable ordering!
     * @param builders The SAT problem as a choco constraint (from a constraint builder)
     * @return Solution for an abstract problem (using choco constraints).
     */
  def solveChocoBool(builders: Iterable[ConstrBuilder]) : Option[GCBoolSolution] = {
    val choSol = ChoConstraints(builders).solve(da.guessOrder,buf)
    for (s <- choSol) yield new GCBoolSolution(s.sol2map)
  }

  def incrementAndSolve(builders: Iterable[ConstrBuilder],
                        sol: GCBoolSolution,
                        pEval: PEval) : (Option[GCBoolSolution],Iterable[ConstrBuilder]) = {
    //                        sdelta: Map[String,Int],
    //                        srest: Set[MutSet[String]]) : Option[GCBoolSolution] = {// (Map[String,Int], Set[MutSet[String]]) = {
    // for each remaining group, negate it in the constraints
    //var newCnf = cnf
    var newBuilders = builders.toSet
    for (group <- pEval.flattenRest) {
      var avoid: ConstrBuilder = FalseC //List[Int]()
      for (v <- group) {
        // TODO: Check if FUNCTIONS need to be considered.
        if (da.max contains v) {
          for ((pred,fs) <- da.domain(v)) {
            val pvar = predVar(v,pred,fs)
            avoid = avoid or (if (sol.hasFlow(pvar)) common.beh.choco.Neg(common.beh.choco.Var(pvar))
                              else common.beh.choco.Var(pvar))
          }
          newBuilders = newBuilders + avoid
          avoid = FalseC
        }
      }
    }
    (solveChocoBool(newBuilders),newBuilders)
  }

  ///////////////////////

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

  def +(other: GuardedCom): GuardedCommands = {
    val thiscommands = commands
    new GuardedCommands(){
      commands = thiscommands + other
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