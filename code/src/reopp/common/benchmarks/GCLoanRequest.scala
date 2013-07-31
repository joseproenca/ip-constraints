package reopp.common.benchmarks

import reopp.common.guardedcommands._
import reopp.common.{Utils, IntPredicate}
import Utils._
import reopp.common.guardedcommands.dataconnectors._
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import reopp.common.IntPredicate

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 19/06/12
 * Time: 20:03
 * To change this template use File | Settings | File Templates.
 */

class GCLoanRequest

object GCLoanRequest extends App {

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
          else               10
  val choco = if (args.size > 1) args(1) startsWith "c"
              else               false
  val justInit = if (args.size > 2) args(2) startsWith "i"
                 else               false

  Warmup.go

  val baseProblem: Formula =  // stateless part
    new GCSDrain("req1","req2",0).getConstraints ++
      new GCSDrain("appr1","appr2",0).getConstraints ++
      new GCIMerger("req2","auth2","pin",0).getConstraints ++
      new GCMerger("denied","appr2","out",0).getConstraints ++
      // filters
      new GCFilter("login","auth",0,IntPred(dataVar("login",0),new Authorised)).getConstraints ++
      new GCFilter("isEn","denied",0,IntPred(dataVar("isEn",0),new Deny)).getConstraints ++
      new GCFilter("isEn","appr1",0,IntPred(dataVar("isEn",0),new Approve)).getConstraints

  var problems: List[Formula] = for (bankclerk <- List(1,3)) yield
      baseProblem ++ // init state
      new GCWriter("start",0,List(3)).getConstraints ++
      new GCWriter("login",0,List(bankclerk)).getConstraints ++
      new GCFifo("start","isEn",0).getConstraints ++
      new GCFifo("start","req1",0).getConstraints ++
      new GCFifo("auth","auth2",0).getConstraints ++
      new GCFifo("pin","appr2",0).getConstraints ++
      Formula(True --> Var(flowVar("start",0))) ++ // force data on start
      Formula(True --> Var(flowVar("login",0)))    // and on login

  // after success of 1
  problems :::= List(baseProblem ++
    new GCWriter("start",0).getConstraints ++
    new GCWriter("login",0).getConstraints ++
    new GCFifo("start","isEn",Some(3),0).getConstraints ++
    new GCFifo("start","req1",Some(3),0).getConstraints ++
    new GCFifo("auth","auth2",Some(1),0).getConstraints ++
    new GCFifo("pin","appr2",0).getConstraints ++
    Formula(True --> Var(flowVar("pin",0)))    // force data on IMerger
  )

  // after success of only login
  problems :::= List(baseProblem ++
    new GCWriter("start",0).getConstraints ++
    new GCWriter("login",0).getConstraints ++
    new GCFifo("start","isEn",0).getConstraints ++
    new GCFifo("start","req1",0).getConstraints ++
    new GCFifo("auth","auth2",Some(1),0).getConstraints ++
    new GCFifo("pin","appr2",0).getConstraints ++
    Formula(True --> Var(flowVar("pin",0)))    // force data on IMerger
  )

  // if only IMerger had flow before
  problems :::= (for (client <- List(1,2,3)) yield baseProblem ++
    new GCWriter("start",0).getConstraints ++
      new GCWriter("login",0).getConstraints ++
      new GCFifo("start","isEn",Some(client),0).getConstraints ++
      new GCFifo("start","req1",0).getConstraints ++
      new GCFifo("auth","auth2",0).getConstraints ++
      new GCFifo("pin","appr2",Some(1),0).getConstraints ++
      Formula(True --> Var(flowVar("isEn",0)))) // force data before filters

  var time: Long = 0
  var res: Option[GCSolution] = None
  var spent: Long = 0
  var total: Long = 0

  for (prob <- problems; run <- 1 to n) {
      if (justInit) prob.justInit = true

      if (choco) {
        time  = System.currentTimeMillis()
        res   = prob.solveChocoSat
        spent = System.currentTimeMillis() - time
      }
      else {
        time  = System.currentTimeMillis()
        res   = prob.solveIterative
        spent = System.currentTimeMillis() - time
      }
      total += spent
      print(", "+spent)
  }
  println("\nAverage: "+(total / (n*7)))

//  val time1 = System.currentTimeMillis()
//  val res1 = problem1.solveIterative
//  val spent1 = System.currentTimeMillis() - time1
//
//  val time2 = System.currentTimeMillis()
//  val res2 = problem2.solveIterative
//  val spent2 = System.currentTimeMillis() - time2

//  val time3 = System.currentTimeMillis()
//  val res3 = problem3.solveIterative
//  val spent3 = System.currentTimeMillis() - time3
//
//  if (res3.isDefined)  println(res3.get.pretty)

//  if (res1.isDefined) println("PAS solved in "+spent1+" ms.")
//  else println("PAS - no solution (in "+spent1+" ms)")
//
//  if (res2.isDefined) println("PAS solved in "+spent2+" ms.")
//  else println("PAS - no solution (in "+spent2+" ms)")
//
//  if (res3.isDefined) println("PAS solved in "+spent3+" ms.")
//  else println("PAS - no solution (in "+spent3+" ms)")

  //    if (res.isDefined) print(spent+" ")
  //    else print("- ")


  ///// PREDICATES ///////

  class Authorised extends IntPredicate {
    val choPred = (x:IntegerExpressionVariable) => Choco.or(Choco.eq(x,1),Choco.eq(x,2))
    val funPred = (x:Int) => (x == 1) || (x == 2)
    override def toString = "Authorised"

    val z3Pred = null
  }



  class Deny extends IntPredicate {
    val sd = Choco.makeIntVar("d_s")
    val ad = Choco.makeIntVar("d_d")
    val pd = Choco.makeIntVar("d_p")
    val choPred = (x:IntegerExpressionVariable) => //throw new Exception("choPred not implemented")
        Choco.and(
          Choco.implies(Choco.eq(x,1),Choco.and(
            Choco.eq(sd,1100), Choco.eq(ad,10000), Choco.eq(pd,2) )),
          Choco.implies(Choco.eq(x,2),Choco.and(
            Choco.eq(sd,2100), Choco.eq(ad,20000), Choco.eq(pd,15) )),
          Choco.implies(Choco.eq(x,3),Choco.and(
            Choco.eq(sd,1850), Choco.eq(ad,150000), Choco.eq(pd,10) )),
          Choco.implies(Choco.and(Choco.not(Choco.eq(x,1)),Choco.not(Choco.eq(x,2)),Choco.not(Choco.eq(x,3))),
              Choco.and( Choco.eq(sd,2000), Choco.eq(ad,1000), Choco.eq(pd,10) )),
        Choco.lt(sd,Choco.mult(2,Choco.div(ad,Choco.mult(pd,12))))
        )

    val funPred = (x:Int) => salary(x) < (2 * (amount(x) / (period(x)*12)))
    override def toString = "Deny"

    // none 1, approve 2, deny 3
    val salary = Map(1 -> 1100 , 2 -> 2100 , 3 -> 1850)   withDefaultValue 2000
    val amount = Map(1 -> 10000, 2 -> 20000, 3 -> 150000) withDefaultValue 1000
    val period = Map(1 -> 2    , 2 -> 15   , 3 -> 10)     withDefaultValue 10

    val z3Pred = null
  }

  class Approve extends IntPredicate {
    val sd = Choco.makeIntVar("d2_s")
    val ad = Choco.makeIntVar("d2_d")
    val pd = Choco.makeIntVar("d2_p")

    val choPred = (x:IntegerExpressionVariable) => //throw new Exception("choPred not implemented")
      Choco.and(
        Choco.implies(Choco.eq(x,1),Choco.and(
          Choco.eq(sd,1100), Choco.eq(ad,10000), Choco.eq(pd,2) )),
        Choco.implies(Choco.eq(x,2),Choco.and(
          Choco.eq(sd,2100), Choco.eq(ad,20000), Choco.eq(pd,15) )),
        Choco.implies(Choco.eq(x,3),Choco.and(
          Choco.eq(sd,1850), Choco.eq(ad,150000), Choco.eq(pd,10) )),
        Choco.implies(Choco.and(Choco.not(Choco.eq(x,1)),Choco.not(Choco.eq(x,2)),Choco.not(Choco.eq(x,3))),
          Choco.and( Choco.eq(sd,2000), Choco.eq(ad,1000), Choco.eq(pd,10) )),
        Choco.gt(sd,Choco.mult(3,Choco.div(ad,Choco.mult(pd,12))))
      )

    val funPred = (x:Int) => salary(x) > (3 * (amount(x) / (period(x)*12)))
    override def toString = "Approve"

    // none 1, approve 2, deny 3
    val salary = Map(1 -> 1100 , 2 -> 2100 , 3 -> 1850)   withDefaultValue 2000
    val amount = Map(1 -> 10000, 2 -> 20000, 3 -> 150000) withDefaultValue 1000
    val period = Map(1 -> 2    , 2 -> 15   , 3 -> 10)     withDefaultValue 10

    val z3Pred = null
  }

}
