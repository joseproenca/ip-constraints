package reopp.common.guardedcommands

import _root_.z3.scala.Z3Context
import reopp.common.guardedcommands.chocobuilder.{ChocoBuilderInt, ChocoBuilderSAT}
import chocodyn.{ChocoDyn, DynSolution}
import chocox.{ChocoX, CXSolution}
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt
import org.sat4j.specs.{TimeoutException, ContradictionException, ISolver}
import collection.mutable.{Set => MutSet, Map => MutMap, ListBuffer}
import reopp.common._
import Utils._
import reopp.common.choco.{ConstrBuilder,ChoSolution,FalseC}
import reopp.common.guardedcommands.z3.{XZ3, Z3Solution, Z3}
import reopp.common
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 18:14
 * To change this template use File | Settings | File Templates.
 */

class Formula extends Constraints[GCSolution,Formula] {

  private var log = System.out
//  val log = new java.io.PrintStream(new java.io.FileOutputStream("/dev/null"))
  var justInit = false

  var commands = Set[GuardedCom]()
  //  var eqvars = Set[(String,String)]()
  private val da = DomainAbst()
  private var solvedDomain = false
  //  var someVars: Option[MutMap[String,Int]] = None

  private var closed = false


  //  var buf = new Buffer
//  private var buf: Option[Buffer] = None
//  def getBuf: Option[Buffer] = buf

  override def toString = commands.mkString("\n")


  /**
   * Default solving approach.
   * @return possible data solution
   */
//  def solve = lazyDataSolve  // predicate abstraction + choco (using guesses)
//  def solve = solveChocoX    // choco - 1st attempt (predicates that keep track of all functions)
//  def solve = solveChocoDyn  // choco - 2nd attempt (dynamic mapping between ints (in choco) and the value they represent)
  def solve = solveXZ3       // z3    - same as chocoDyn. Tricks used to be able to recover solution and to avoid incomplete theories (not possible to say "if A is instantiated...")

  /**
   * Collect the domain of every guarded command in field 'da'.
   * Used by 'collectVars'.
   */
  private def solveDomain() {
    if (!solvedDomain) {
      for (c <- commands) c.solveDomain(da)
//      println(da.pp)
//      println(da.guessOrder)
      solvedDomain = true
    }
  }

  /**
   * Collect the domain invariants of every guarded command.
   * @return the domain invariants
   */
  def getDA: DomainAbst = {
    solveDomain()
    da
  }



  /**
   * Combines the free variables of all guarded commands
   * @return free variables of all guarded commands
   */
  def fv() = commands.map(_.fv).foldRight[Set[String]](Set())(_++_)
  /**
   * Collects the free variables of all guarded commands, using an accumulator for efficiency.
   * @return free variables of all guarded commands
   */
  def fv2: Iterable[String] = {
    val res = MutSet[String]()
    for (c <- commands) c.fv2(res)
    res
  }
  /**
   * Combines the *boolean* free variables of all guarded commands efficiently
   * @return boolean free variables of all guarded commands
   */
  def bfv(l:ListBuffer[String]) { for (c<-commands) c.bfv(l) }
  //commands.map(_.bfv).foldRight[Set[String]](Set())(_++_)

  /**
   * Should be performed only once per guarded command.
   * Possible optimization: collect vars upon creation
   *   - ignored since lots of temporary domains are calculated.
   * @return Map from var names to their unique id number from 1 to size(map) - to be used by a SAT solver.
   */
  def collectVars: MutMap[String, Int] = {
//    if (someVars.isDefined) return someVars.get

    solveDomain()

    val vars2 = MutMap[String,Int](""->1)
    for (c <- commands) c.afv2(da,vars2)
    vars2 -= ""

//    println("VARS: old/new "+(t2-t1)+"/"+(t3-t2))
//    println("V1: "+vars.toList.sorted)
//    println("V2: "+vars2.toList.sorted)


    // TODO: optimize constraints: search for eq vars and assign the same var (int)!

    vars2
  }


  /**
   * Collects the SAT problem for the abstract problem. Uses 'collectVars' (and indirectly 'solveDomain').
   * @return  The CNF problem for the abstract problem.
   */
  def toCNF: (CNF.Core,MutMap[String,Int]) = {
//    val t0 = System.currentTimeMillis()
    val vars: MutMap[String, Int] = collectVars

//    // DEBUGGING
//    val fv = commands.map(_.fv).foldRight[Set[String]](Set())(_++_)
//    println("fv: "+fv.mkString(","))
//    println("afv: "+afv.mkString(","))
//    println("DA: "+da.pp)
//    for (v <- fv)
//      if (!da.domain(v).isEmpty)
//        println("var "+v+" - domain: "+da.domain(v).mkString(","))

//    val t1 = System.currentTimeMillis()
//    val cnf1 = commands.map(_.toCNF(vars,da)).foldRight[CNF.Core](List())((x:CNF.Core,y:CNF.Core) => x ::: y)
//    val t2 = System.currentTimeMillis()
    val cnf2tmp: CNF2.Core = new ListBuffer()
    for (c <- commands) c.toCNF2(vars,da,cnf2tmp)
    val cnf2 = cnf2tmp.toList
//    val t3 = System.currentTimeMillis()
//
//    println("CNFS: vars/old/new - "+(t1-t0)+"/"+(t2-t1)+"/"+(t3-t2))

//    val c11 = cnf1.map(_.toList.sorted)
//    val c22 = cnf2.map(_.toList.sorted)
//    println("CNF1: "+c11.sorted(Ordering[Iterable[Int]]))
//    println("CNF2: "+c22.sorted(Ordering[Iterable[Int]]))

    (cnf2,vars)
  }

  /**
   * Add SOME-FLOW and CC3 constraints.
   * CC3 not yet in use.
   */
  @deprecated
  private def closeOld() {
    if (!closed) {
      var flowvars: Guard = !True
      for (v <- fv())
        if (isFlowVar(v)) {
          flowvars = (Var(v) \/ flowvars)
//          commands +=  Var(v) \/ Var(flow2snk(v)) \/ Var(flow2src(v))
        }
      //if (useCC3)
      commands += flowvars
      closed = true
    }

  }

  /**
   * Add SOME-FLOW and CC3 constraints.
   * CC3 not yet in use.
   * Used before using Choco (predicate abstraction + lazy & Choco as SMT).
   * Other approaches (Z3 and SAT4J) use optimised version.
   */
  def close() {
//    println("## closing...")
    if (!closed) {
//      println("## not closed yet...")
      val bvars = new ListBuffer[String]()
      bfv(bvars)
//      println("## b vars: "+bvars.size)
      if (!bvars.isEmpty) {
        val fst = bvars.head
        var flowConstraint: Guard = Var(fst)
        for (v <- bvars.tail)
          flowConstraint = flowConstraint \/  Var(v)
//        println("closing - adding "+flowConstraint)
        commands += flowConstraint
      }
      closed = true
    }
  }

  def flatten() {

  }



  /**
   * Calculates a partial evaluation based on a solution to its abstract problem.
   * @param sol Solution to this' abstract problem
   * @return A partial evaluation - initially only a set of (var and data) assignments and function applications.
   */
  def partialEval(sol:Solution): PEval = { //(Map[String,Int], Map[String,Set[String]]) = {
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


  /**
   * Predicate abstraction + SAT + loop until data solution is found.
   * @return Data solution for the constraints.
   */
  def solveIterative : OptionSol[GCSolution] = {
    // solveDomain; toCNF; solveBool?; [partialEval; quotient; dataAssign(done?); solveData(done?); incrementAndSolve?]
    val t1 = System.currentTimeMillis()
    val cnf = toCNF
//    val t2 = System.currentTimeMillis()
    val optSolBool = solveBool(cnf._1,cnf._2)
//    val t3 = System.currentTimeMillis()
//    println("[SAT toCNF-Solve] "+(t2-t1)+" - "+(t3-t2))
    if (!optSolBool.isDefined)
      return NoneSol()
//    println("INIT GUESS:\n"+optSolBool.get.pretty)

    if (justInit) {
      return SomeSol(new GCSolution(optSolBool.get,Map[String, Integer]()))
    }

    val res = loopPartialEval(partialEval(optSolBool.get),cnf,optSolBool.get,0)
//    log.println("[all solve] "+(System.currentTimeMillis()-time))
    res
  }

  //private var counter
  private def loopPartialEval(pEval: PEval,cnf: (CNF.Core,MutMap[String,Int]), solBool: GCBoolSolution,time: Long): OptionSol[GCSolution] = {
//    println("#> solved  pEval             - "+pEval)
    pEval.quotient()
//    println("#> calculated quotient       - "+pEval)
//    log.println("[quotient] "+(System.currentTimeMillis()-time))
    pEval.applyDataAssgn(solBool)
//    println("#> solved simple data assign - "+pEval)
//    log.println("[apply data] "+(System.currentTimeMillis()-time))
    if (pEval.isFinished) return SomeSol(pEval.getSol(solBool))
    pEval.solveSimpleData(solBool,da)
//    println("#> solved domain elements    - "+pEval)
    if (pEval.isFinished) return SomeSol(pEval.getSol(solBool))
    val (optSol2,newcnf) = incrementAndSolve(cnf._1,cnf._2,solBool,pEval)
//    println("#> incremented and solved new constr.")//+newcnf)
    if (!optSol2.isDefined) return NoneSol()

//    println("#> restarting loop...") //        - "+optSol2.get.pretty)
    loopPartialEval(partialEval(optSol2.get),newcnf,optSol2.get,time)
  }


  /**
   * Predicate abstraction + SAT
   * @return solution for the predicate abstraction
   */
  def solveBool: Option[GCBoolSolution] = {
    val (cnf,vars) = toCNF
    solveBool(cnf,vars)
  }

  /**
   * Perform SAT over a given abstracted predicate (static function)
   * @param c abstracted predicate in CNF form
   * @param vars mapping between variable names and integers in the CNF
   * @return solution for the predicate abstraction
   */
  private def solveBool(c: CNF.Core, vars: MutMap[String,Int]): Option[GCBoolSolution] = {

    // store variable names
    val varname = MutMap[Int,String]()
    for ((k,v) <- vars)
      varname(v) = k



    // ADD FLOW CONSTRAINTS! (if necessary)
    var cnf = c
    if (!closed) {
      var flowvars = Set[Int]()
      for ((k,v) <- vars)
        if (isFlowVar(k)) flowvars += v
      cnf = flowvars.toArray :: c
    }


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
        Some(new GCBoolSolution(res,None))
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


  ////////////////////////////
  // OPTMISING - NOT IN USE //
  ////////////////////////////

  def optimiseEqVars: Map[String,String] = {
    val vars = MutMap[String,String]()
    for (gc <- commands)
      gc.optimiseEqVars(vars)

    vars
    // TODO: under construction
    throw new RuntimeException("Under construction")
  }

  /////////////////////////////////
  // USING DETERMINED CONNECTORS //
  /////////////////////////////////

  /**
   * Predicate abstraction + SAT + direct data solution (no iteration)
   * Assumes determined and closed connectors.
   * @return Data solution or 'None' if there is no solution (maybe because the assumption does not hold.)
   */
  def quickDataSolve : OptionSol[GCSolution] = {
    val t0 = System.currentTimeMillis()
    solveDomain()

    val t1 = System.currentTimeMillis()
    val cnf = toCNF
    val t2 = System.currentTimeMillis()
    // optimised closing (for flow variables) in solveBool for CNF's
    val optSolBool = solveBool(cnf._1,cnf._2)
    val t3 = System.currentTimeMillis()
//    println("[QSAT Domain-toCNF-Solve] "+(t1-t0)+" - "+(t2-t1)+" - "+(t3-t2))
    if (!optSolBool.isDefined) {
      println("Fail boolean satisfaction...")
      return NoneSol()
    }

    val pEval = partialEval(optSolBool.get)
    val done = pEval.freshTraversal(None)

    if (done)
      SomeSol(pEval.getSol(optSolBool.get))
    else {
      println("failed... peval:\n"+pEval.toString)
      println("failed... boolSol:\n"+optSolBool.get)
      NoneSol()
    }
  }

  /**
   * Same as `Formula.quickDataSolve()`, but using Z3 instead of SAT4J for SAT solving.
   *
   * @see # quickDataSolve
   * @param z3 context required by Z3
   * @return Data solution
   */
  def quickDataSolve(z3: Z3Context): OptionSol[GCSolution] = {
    val t0 = System.currentTimeMillis()
    solveDomain()
//    close  // OPTMISED in gc2boolz3, based on this.bfv

    val t1 = System.currentTimeMillis()
    val z3term = Z3.gc2boolz3(this,z3)
//    println("z3 term: "+z3term.toString())
    val optSolBool = Z3.solvez3(z3term,z3)
    val t3 = System.currentTimeMillis()
//    println("[QZ3  Domain-toBoolZ3+Solve] "+(t1-t0)+" - "+(t3-t1))
    if (!optSolBool.isDefined)
      return NoneSol()

    val pEval = partialEval(optSolBool.get)
    val done = pEval.freshTraversal(None)

    if (done)
      SomeSol(pEval.getSol(optSolBool.get))
    else {
//      println("failed... peval:\n"+pEval.toString)
//      println("failed... boolSol:\n"+optSolBool.get.pretty)
      NoneSol()
    }
  }


  /**
   *  Predicate abstraction + Optimised Choco (as SAT) + direct data solution (no iteration).
   *  Assumes connector is determined and closed.
   *  Optimised choco: Lazy constraints and smart variable ordering.
   * @return Data solution
   */
  def lazyDataSolve : OptionSol[GCSolution] = {
//    buf = Some(new Buffer)
    close()
    solveDomain()

    val builders = ChocoBuilderSAT.gc2BoolConstrBuilders(this,da)
//    println("boolean constraints: \n"+builders.mkString("\n"))
//    val buf = new Buffer // using the same to solve boolean constraints and to get a solution while traversing the tree.
    val optSolBool = ChocoBuilderSAT.solveChocoBool(da,builders)

    if (!optSolBool.isDefined)
      return NoneSol(optSolBool.getBuffer)

    val buf = optSolBool.getBuffer // MUST exist after solveChocoBool

    val pEval = partialEval(optSolBool.get)
    val done = pEval.freshTraversal(buf)

    if (done){
      val sol = pEval.getSol(optSolBool.get)
//      sol.buf = buf
      SomeSol(sol)
    }
    else
      NoneSol(buf)
  }

//  /**
//   * Predicate abstraction + Choco (as SAT)
//   * Optimised Choco: lazy constraints + variable ordering
////   * Requires buf != None
//   * @return Boolean solution for the abstract constraints.
//   */
//  def solveChocoBool : Option[GCBoolSolution] = {
//    buf = Some(new Buffer)
//    close()
//    solveDomain()
//    val builders = ChocoBuilderSAT.gc2BoolConstrBuilders(this,da)
//    //    println("#> solving abst using choco SAT cnf - "+da.pp)
//    //    println("builder: "+builders.mkString("\n"))
//    solveChocoBool(builders)
//  }
//
//  /** Requires buf != None */
//  private def solveChocoBool(builders: Iterable[ConstrBuilder]) : Option[GCBoolSolution] = {
//    if (!buf.isDefined) throw new RuntimeException("Required buffer not defined.")
//    val choSol = ChoConstraints(builders).solve(da.guessOrder,buf.get)
//    for (s <- choSol) yield new GCBoolSolution(s.sol2map)
//  }


  /**
   * Solve solutions using Choco and external predicates/functions, based on book-keeping of hash values.
   * @return Solution for the data constraints.
   */
  def solveChocoX: OptionSol[CXSolution] = {
//    ChocoX.solve(getConstraints)
    close()
    ChocoX.solve(this)
  }

  /**
   * Solve solutions using Choco and external predicates/functions, based on a dynamic map from ints to data values.
   * @return Solution for the data constraints.
   */
  def solveChocoDyn: OptionSol[DynSolution] = {
    //    ChocoX.solve(getConstraints)
    close()
    ChocoDyn.solve(this)
  }

  /**
   * Experimental - solve solutions using Z3 and external predicates/functions, using a custom theory, based on a dynamic map from ints to data values.
   * @return Solution for the data constraints.
   */
  def solveXZ3: OptionSol[GCSolution] = {
    close()
    XZ3.solvexz3(this)
  }

  /////////////////////////
  // USING CHOCO FOR SMT //
  /////////////////////////

  /**
   * Choco as SMT
   * @return Data constraints
   */
  def solveChoco : OptionSol[ChoSolution] = {
    // not closing constraints
    // NOW closing
    close()
    ChocoBuilderInt.solveChoco(this)
  }


  def solvez3: OptionSol[Z3Solution] = {
    Z3.solvez3(this)
  }




  /////////////////////////
  // USING CHOCO FOR SAT //
  /////////////////////////

  /**
   * Predicate abstraction + Choco (as SAT solver) + loop until data solution is found
   * @return Data solution
   */
  def solveChocoSat : OptionSol[GCSolution] = {
    solveDomain()
    // solveDomain; toCNF; solveBool?; [partialEval; quotient; dataAssign(done?); solveData(done?); incrementAndSolve?]
    val time = System.currentTimeMillis()
    val builders = ChocoBuilderSAT.gc2BoolConstrBuilders(this,da)
//    println("#> solving abst using choco SAT cnf - "+da.pp)
//    println("builder: "+builders.mkString("\n"))
    val optSolBool = ChocoBuilderSAT.solveChocoBool(da,builders)

//    log.println("[Cho/SAT solve] "+(System.currentTimeMillis()-time))
    if (!optSolBool.isDefined)
      return  NoneSol(optSolBool.getBuffer)
//    println("INIT GUESS:\n"+optSolBool.get.pretty)

    if (justInit) return SomeSol(new GCSolution(optSolBool.get,Map[String, Integer]()))

    val res = loopPartialEvalCho(partialEval(optSolBool.get),builders,optSolBool.get)
//    log.println("[all solve]     "+(System.currentTimeMillis()-time))
//    if (res.isDefined) res.get.buf = buf
    res
  }

  private def loopPartialEvalCho(pEval: PEval,builders: Iterable[ConstrBuilder], solBool: GCBoolSolution)
      : OptionSol[GCSolution] = {
//    println("#> solved  pEval             - "+pEval)
    pEval.quotient()
//    println("#> calculated quotient       - "+pEval)
    pEval.applyDataAssgn(solBool)
//    println("#> solved simple data assign - "+pEval)
    if (pEval.isFinished) return SomeSol(pEval.getSol(solBool))
    pEval.solveSimpleData(solBool,da)
//    println("#> solved domain elements    - "+pEval)
    if (pEval.isFinished) return SomeSol(pEval.getSol(solBool))
    val (optSol2,newbuilders) = incrementAndSolve(builders,solBool,pEval)
//    println("#> incremented and solved new constr.")//+newcnf)
    if (!optSol2.isDefined) return NoneSol(solBool.getBuffer)

//    println("#> restarting loop...") //        - "+optSol2.get.pretty)
    loopPartialEvalCho(partialEval(optSol2.get),newbuilders,optSol2.get)
  }

  private def incrementAndSolve(builders: Iterable[ConstrBuilder],
                                sol: GCBoolSolution,
                                pEval: PEval) : (OptionSol[GCBoolSolution],Iterable[ConstrBuilder]) = {
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
            avoid = avoid or (if (sol hasFlowOn pvar) common.choco.Neg(common.choco.Var(pvar))
            else common.choco.Var(pvar))
          }
          newBuilders = newBuilders + avoid
          avoid = FalseC
        }
      }
    }
    // BUFFER needed to be defined outside ChochBuilderSAT, because of the incrementing of the solution.
    (ChocoBuilderSAT.solveChocoBool(sol.getBuffer.get,da,newBuilders),newBuilders)
  }




  ///////////////////////

  def ++(other: Formula): Formula = {
    val thiscommands = commands
    new Formula(){
      commands = thiscommands ++ other.commands
    }
  }
  def ++(others: Iterable[GuardedCom]): Formula = {
    val thiscommands = commands
    new Formula(){
      commands = thiscommands ++ others
    }
  }

  def ++(other: GuardedCom): Formula = {
    val thiscommands = commands
    new Formula(){
      commands = thiscommands + other
    }
  }
}


object Formula {
  def apply(gs: Iterable[GuardedCom]): Formula = {
    val g = new Formula()
    g.commands = gs.toSet
    g
  }
  def apply(gs: GuardedCom*): Formula = apply(gs.toSet)
  def apply(g: GuardedCom): Formula = apply(Set(g))
  def apply(): Formula = new Formula()

}