package common.beh.guardedcommands

import common.beh.Constraints
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt
import org.sat4j.specs.{TimeoutException, ContradictionException, ISolver}
import scala.collection.mutable.{Set => MutSet, Map => MutMap}
import common.beh.Utils._
import common.beh.choco._
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

  var commands = Set[GuardedCom]()
  var da = DomainAbst()


  def solveDomain() {
    for (c <- commands) da = da + c.da
  }

  def toCNF: (CNF.Core,MutMap[String,Int]) = {
    solveDomain()

    val afv = commands.map(_.afv(da)).foldRight[Set[String]](Set())(_++_)
    var i = 1
    var vars = MutMap[String,Int]()
    for (v <- afv) {
      vars(v) = i
      i += 1
    }

    // build mapping of predicates
//    val fv = commands.map(_.fv).foldRight[Set[String]](Set())(_++_)
//    for (v <- fv)
//      if (!da.domain(v).isEmpty)
//        preds += v -> domain
//            println("var "+v+" - domain: "+da.domain(v).mkString(","))


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

  //type PEval = (Map[String,Int], Map[String,String])

  def partialEval(sol:GCBoolSolution): PEval = { //(Map[String,Int], Map[String,Set[String]]) = {
    val pevals = commands.map(_.partialEval(sol)) // list of pairs of maps
    val peval = pevals.foldRight[PEval](new PEval(Map(),Map()))((x:PEval,y:PEval) => x++y)//(x._1++y._1, x._2++y._2))
    peval
  }

//  def quotient(peval: PEval): Map[String,MutSet[String]] = {
//
//    class Wrapper(var set: MutSet[String])
//
//
//    var res = Map[String,Wrapper]()
//
//    for ((x,y) <- peval._2) {
//      if ((res contains x) && (res contains y)) {
//        for (yv <- res(y).set) res(x).set.add(yv) // ++= res(y).set
//        res(y).set = res(x).set
//      }
//      else if (res contains x) {
//        res(x).set.add(y)
//        res += y -> res(x)
//      }
//      else if (res contains y) {
//        res(y).set.add(x)
//        res += x -> res(y)
//      }
//      else {
//        res += x -> new Wrapper(MutSet[String](x,y))
//        res += y -> res(x)
//      }
////      println("added "+(x,y)+" - "+(for ((a,s) <- res) yield a + " +> "+s.set.mkString("{",",","}") ))
//    }
//
//    for ((a,b) <- res) yield a -> b.set
//  }

//  def applyDataAssgn(sol:GCBoolSolution): (Map[String,Int], Set[MutSet[String]]) = { // Map[String,MutSet[String]]) = {
//    val peval = partialEval(sol)
//    var partition = peval.quotient() //quotient(peval)
//    var res = Map[String,Int]()
//
//    for ((x,int) <- peval._1) {
//      if (partition contains x) {
//        for (v <- partition(x)) {
//          res += v -> int
//          partition -= v
//        }
//      }
//      else
//        res += x -> int
//    }
//
//    // extend partition with variables with dataflow, not in the leftovers of the partition
//    for ((v,bool) <- sol.varMap)
//      if (isFlowVar(v) && bool)
//        if (!(res contains flow2data(v)) && !(partition contains flow2data(v)))
//            partition += v -> MutSet(v)
//
//    // drop indexes of the partition
//    (res,partition.values.toSet)
//  }

//  def solveSimpleData(sol:GCSolution, sdelta: Map[String,Int], srest: Set[MutSet[String]]) : (Map[String,Int], Set[MutSet[String]]) = {
//    var rest = srest
//    var delta = sdelta
//    for (group <- srest) {
//      var preds = Set[ConstrBuilder]()
//      for (v <- group) {
//        if (da.max contains v)
//          for (pred <- da.domain(v)) {
//            val pvar = predVar(v,pred)
//            preds += (if (sol(pvar)) FlowPred(pred.choPred,"x")
//                      else common.beh.choco.Neg(FlowPred(pred.choPred,"x")))
//          }
//      }
////      preds ++= da.domain(v)
//      println("solving group "+group.mkString("{",",","}")+
//              " by adding to predicates "+preds.mkString("{",",","}"))
//      if (preds.isEmpty) {
//        for (v <- group) delta += v -> 0
//        rest -= group
//      }
//      else {
//        val c = new ChoConstraints()
//        c impose preds //(for (p <- preds) yield FlowPred(p.choPred,"x"))
//        val sol = c.solve
//        if (sol.isDefined) {
//          rest -= group
//          for (v <- group) delta += v -> sol.get.getVal("x").get
////                  print(sol.get.pretty)
////                  println("solved solution for group "+group.mkString("{",",","}"))
//        }
//
//      }
//    }
//    (delta,rest)
//  }

  def incrementAndSolve(cnf: CNF.Core,
                        vars: MutMap[String,Int],
                        sol: GCBoolSolution,
                        pEval: PEval) : (Option[GCBoolSolution],(CNF.Core,MutMap[String,Int])) = {
    //                        sdelta: Map[String,Int],
    //                        srest: Set[MutSet[String]]) : Option[GCBoolSolution] = {// (Map[String,Int], Set[MutSet[String]]) = {
    // for each remaining group, negate it in the constraints
    var newCnf = cnf
    for (group <- pEval.flattenRest) {
      var avoid = List[Int]()
      for (v <- group) {
        if (da.max contains v) {
          for (pred <- da.domain(v)) {
            val pvar = predVar(v,pred)
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


//    // from here: extend the data with information from ':='
//
//    var vareqs = Map[String,Set[String]]().withDefaultValue(Set[String]())
//    // invert mapping of assignments!
//    for ((_,m) <- pevals; (v1,v2) <- m)
//      vareqs += v2 -> (vareqs(v2) + v1)
//
//
//    var initData = Map[String,Int]()
//    for ((m,_) <- pevals)
//      initData ++= m
//
//    var newData = initData
//    while (!newData.isEmpty) {
//      val (s,d) = newData.head
//      newData = newData.tail
//      if (vareqs contains s)
//        for (v2 <- vareqs(s)) {
//          newData += v2 -> d
//          initData += v2 -> d
//        }
//    }
//    (initData,vareqs)
//  }


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

    val res = loopPartialEval(partialEval(optSolBool.get),cnf,optSolBool.get,time)
//    log.println("[all solve] "+(System.currentTimeMillis()-time))
    res
  }

  //private var conter
  private def loopPartialEval(pEval: PEval,cnf: (CNF.Core,MutMap[String,Int]), solBool: GCBoolSolution,time: Long): Option[GCSolution] = {
//    println("#> solved  pEval             - ")//+pEval)
    pEval.quotient()
//    println("#> calculated quotient       - ")//+pEval)
//    log.println("[quotient] "+(System.currentTimeMillis()-time))
    pEval.applyDataAssgn(solBool)
//    println("#> solved simple data assign - ")//+pEval)
//    log.println("[apply data] "+(System.currentTimeMillis()-time))
    if (pEval.isFinished) return Some(pEval.getSol(solBool))
    pEval.solveSimpleData(solBool,da)
//    println("#> solved domain elements    - ")//+pEval)
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

  def solveBool(cnf: CNF.Core, vars: MutMap[String,Int]): Option[GCBoolSolution] = {

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

  ////////////////////////
  // USING CHOCO FOR SAT

  def solveChocoSat : Option[GCSolution] = {
    // solveDomain; toCNF; solveBool?; [partialEval; quotient; dataAssign(done?); solveData(done?); incrementAndSolve?]
    val time = System.currentTimeMillis()
    val builders = toConstrBuilders
//    println("#> solving abst using choco SAT cnf.") // - "+da.pp)
//    println("builder: "+builders)
    val optSolBool = solveChocoBool(builders)
//    log.println("[Cho/SAT solve] "+(System.currentTimeMillis()-time))
    if (!optSolBool.isDefined)
      return None
//    println("INIT GUESS:\n"+optSolBool.get.pretty)

    val res = loopPartialEvalCho(partialEval(optSolBool.get),builders,optSolBool.get)
//    log.println("[all solve]     "+(System.currentTimeMillis()-time))
    res
  }

  private def loopPartialEvalCho(pEval: PEval,builders: Iterable[ConstrBuilder], solBool: GCBoolSolution): Option[GCSolution] = {
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


  def toConstrBuilders : Iterable[ConstrBuilder] = {
    solveDomain()

    for (com <- commands)
      yield com.toConstrBuilder(da)
  }

  def solveChocoBool(builders: Iterable[ConstrBuilder]) : Option[GCBoolSolution] = {
    val choSol = ChoConstraints(builders).solve
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
        if (da.max contains v) {
          for (pred <- da.domain(v)) {
            val pvar = predVar(v,pred)
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