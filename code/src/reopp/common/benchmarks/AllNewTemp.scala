package reopp.common.benchmarks

import reopp.common.guardedcommands.dataconnectors.ConstraintGen._
import z3.scala.{Z3AST, Z3Config, Z3Context}
import reopp.common.guardedcommands.z3.Z3
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import reopp.common.guardedcommands.Formula
import reopp.common.{OptionSol, Solution, IntFunction, IntPredicate}
/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 13/06/13.
 */
object AllNewTemp extends App {

  /// PARSE ARGUMENTS ///
    Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
  else               3
  val satfull = if (args.size > 1) args(1) startsWith "s"
  else               false
  val chocosat = if (args.size > 1) args(1) startsWith "cs"
  else               false
  val choco = if (args.size > 1) (args(1) startsWith "c") && !chocosat
  else               false
  val z3sat = if (args.size > 1) args(1) startsWith "zs"
  else               false
  val z3 = if (args.size > 1) (args(1) startsWith "z") && !z3sat
  else               false
  val quicksat = if (args.size > 1) args(1) startsWith "q"
  else               false
  val lazyy = if (args.size > 1) args(1) startsWith "l"
  else               false
  val justInit = if (args.size > 2) args(2) startsWith "i"
  else               false

  ///////////////////
  /// The PROBLEM ///
  ///////////////////

  val f2c = new Farhn2Celc
  val isF = new IsFarhn
  val isC = new IsCelc
  val isNight = new IsNight
  val getTime = new GetTime
  val getTemp = new GetTemp


  def genRouters(height:Int): Formula= {
    var snks = List("h")
    var res = Formula()
    for (level <- 1 to height) {
      var newsnks = List[String]()
      for (x <- snks) {
        res ++= exrouter(x,x+"'",x+",")
        newsnks :::= List(x+"'",x+",")
      }
      snks = newsnks
    }

    for (snk <- snks) {
      res ++= transf(snk,snk+"-modified",f2c) ++ reader(snk+"-modified",1)
    }

    (new scala.util.Random).nextDouble()
    res
  }



  def problem =
    transf("a","b",getTime) ++
    negfilter("b","c",isNight) ++
    transf("a","d",getTemp) ++
    filter("d","e",isF) ++
    filter("d","g1",isC) ++
    transf("e","g2",f2c) ++
    merger("g1","g2","g") ++
    sync("g","h") ++
    sdrain("c","h") ++
    writer("a",List(22115)) ++
    genRouters(n)
  // 15 C @ 8:35am --> (8*60 + 35) + 15*1440  =  22115

  //  val problem =
  //    nexrouter("start", (for (x <- 1 to n) yield "display"+x).toList) ++
  //    writer("start",List(1075))


  /////////////////////////
  /// Running the tests ///
  /////////////////////////

  if (justInit) problem.justInit = true


  if (satfull) {
    val time = System.currentTimeMillis()
    val res = problem.solveIterative
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (chocosat) {
    val time = System.currentTimeMillis()
    val res = problem.solveChocoSat
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (choco) {
    val time = System.currentTimeMillis()
    val res = problem.solveChoco
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (z3sat) {
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    val time = System.currentTimeMillis()
    val res = problem.quickDataSolve(z3)
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (z3) {
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    val time = System.currentTimeMillis()
    val res = Z3.solvez3(Z3.gc2z3(problem,z3),z3)
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (quicksat) {
    val time = System.currentTimeMillis()
    val res = problem.quickDataSolve
    val spent = System.currentTimeMillis() - time
    print(spent)
  }
  else if (lazyy) {
    val time = System.currentTimeMillis()
    val res = problem.lazyDataSolve
    val spent = System.currentTimeMillis() - time
    print(spent)
  }

  /// EXPERIMENTS:
  else {

    //    println("  # THE PROBLEM:\n"+problem.commands.mkString(" - ","\n - ","\n"))

    var time: Long = 0
    var res: OptionSol[Solution] = null
    var spent: Long = 0


//    //// QUICK-SAT ////
//    time = System.currentTimeMillis()
//    res = problem.quickDataSolve
//    spent = System.currentTimeMillis() - time
//        if (res.isDefined) println("quick-sat - solved in "+spent+" ms:\n"+res.get)
//        else println("quick-sat - no solution (in "+spent+" ms)")
//    println("quick-sat - "+spent)
//
//    //// SAT-FULL ////
//    time = System.currentTimeMillis()
//    res = problem.solveIterative
//    spent = System.currentTimeMillis() - time
//    //    if (res.isDefined) println("SAT-full - solved in "+spent+" ms:\n"+res.get.pretty)
//    //    else println("SAT-full - no solution (in "+spent+" ms)")
//    println("SAT-full  - "+spent)
//
//    //// SATC-FULL ////
//    time = System.currentTimeMillis()
//    res = problem.solveChocoSat
//    spent = System.currentTimeMillis() - time
//    //    if (res.isDefined) println("SATC-full - solved in "+spent+" ms:\n"+res.get.pretty)
//    //    else println("SATC-full - no solution (in "+spent+" ms)")
//    println("SATC-full - "+spent)
//
//    //// CHOCO ////
//    time = System.currentTimeMillis()
//    res = problem.solveChoco
//    spent = System.currentTimeMillis() - time
//    //    if (res.isDefined) println("Choco - solved in "+spent+" ms:\n"+res.get.pretty)
//    //    else println("Choco - no solution (in "+spent+" ms)")
//    println("Choco     - "+spent)

    /// Z3 ////
    val z3 = new Z3Context(new Z3Config("MODEL" -> true))
    time = System.currentTimeMillis()
    res = Z3.solvez3(Z3.gc2z3(problem,z3),z3)
    spent = System.currentTimeMillis() - time
        if (res.isDefined) println("Z3 - solved in "+spent+" ms:\n"+res.get)
        else println("Z3 - no solution (in "+spent+" ms)")
    println("Z3        - "+spent)

    //// QUICK-SAT-Z3 ////
    val zz3 = new Z3Context(new Z3Config("MODEL" -> true))
    time = System.currentTimeMillis()
    res = problem.quickDataSolve(zz3)
    spent = System.currentTimeMillis() - time
        if (res.isDefined) println("quick-z3  - solved in "+spent+" ms:\n"+res.get)
        else println("quick-z3  - no solution (in "+spent+" ms)")
    println("quick-z3  - "+spent)

//    //// LAZY-SAT ////
//    time = System.currentTimeMillis()
//    res = problem.lazyDataSolve
//    spent = System.currentTimeMillis() - time
//    //    if (res.isDefined) println("lazy-sat - solved in "+spent+" ms:\n"+res.get.pretty)
//    //    else println("lazy-sat - no solution (in "+spent+" ms)")
//    println("lazy-sat  - "+spent)


  }

  class IsFarhn extends IntPredicate {
    // >= 500
    val funPred = (x:Int) => (x >= 500)
    val choPred = (x:IntegerExpressionVariable) => Choco.geq(x,500)
    val z3Pred = (z:Z3Context,v:Z3AST) => z.mkGE(v,z.mkInt(500,z.mkIntSort()))
    override def toString = "IsFarhn"
  }
  class IsCelc extends IntPredicate {
    // < 500
    val funPred = (x:Int) => (x < 500)
    val choPred = (x:IntegerExpressionVariable) => Choco.lt(x,500)
    val z3Pred = (z:Z3Context,v:Z3AST) => z.mkLT(v,z.mkInt(500,z.mkIntSort()))
    override def toString = "IsCelc"
  }
  class IsNight extends IntPredicate {
    // from 7pm (1140) to 7am (420)  x>1140 or x<420
    val choPred = (x:IntegerExpressionVariable) => Choco.or(Choco.geq(x,1140),Choco.lt(x,420))
    val funPred = (x:Int) => (x >= 1140) || (x < 420)
    val z3Pred = (z:Z3Context,v:Z3AST) => //z.mkGT(v,z.mkInt(i,z.mkIntSort()))
      z.mkOr(
        z.mkGE(v,z.mkInt(1140,z.mkIntSort())),
        z.mkLT(v,z.mkInt(420,z.mkIntSort())))
    override def toString = "isNight"
  }


  class Farhn2Celc extends IntFunction {
    //ºC = (ºF  -  32)  x  5/9
    val funFun = (x:Int) => ((x-1032) * 5 )/9
    val choFun = null
    val z3Fun = (z:Z3Context,v:List[Z3AST]) =>
      z.mkDiv(
        z.mkMul(
          z.mkSub(
            v.head,
            z.mkInt(1032,z.mkIntSort())),
          z.mkInt(5,z.mkIntSort())),
        z.mkInt(9,z.mkIntSort()))
    override def toString = "Farhn2Celc"
  }

  class GetTemp extends IntFunction {
    //x = temp*1440 + time
    // time = x % 1440
    // temp = x - temp  =  (x - (x % 1440)) / 1440
    val funFun = (x:Int) => (x - (x % 1440)) / 1440
    val choFun = null
    val z3Fun = (z:Z3Context,v:List[Z3AST]) =>
      z.mkDiv(
        z.mkSub(
          v.head,
          z.mkMod(
            v.head,
            z.mkInt(1440,z.mkIntSort())
          )
        ),
        z.mkInt(1440,z.mkIntSort())
      )
    override def toString = "GetTemp"
  }

  class GetTime extends IntFunction {
    //x = temp*1440 + time
    // time = x % 1440
    // temp = x - temp  =  x - (x % 1440)
    val funFun = (x:Int) => x % 1440
    val choFun = null
    val z3Fun = (z:Z3Context,v:List[Z3AST]) =>
        z.mkMod(
          v.head,
          z.mkInt(1440,z.mkIntSort())
        )
    override def toString = "GetTime"
  }
}
