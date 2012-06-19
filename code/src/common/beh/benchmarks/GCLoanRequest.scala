package common.beh.benchmarks

import common.beh.guardedcommands._
import common.beh.Utils._
import common.beh.guardedcommands.connectors._
import common.beh.choco.dataconnectors.Predicate
import choco.kernel.model.variables.integer.IntegerVariable
import choco.Choco

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 19/06/12
 * Time: 20:03
 * To change this template use File | Settings | File Templates.
 */

class GCLoanRequest

object GCLoanRequest extends App {

  val baseProblem =  // stateless part
    new GCSDrain("req1","req2",0).constraints ++
      new GCSDrain("appr1","appr2",0).constraints ++
      new GCIMerger("req2","auth2","pin",0).constraints ++
      new GCMerger("den","appr2","out",0).constraints ++
      // filters
      new GCFilter("login","auth",0,Pred(new Authorised,dataVar("login",0))).constraints ++
      new GCFilter("isEn","denied",0,Pred(new Deny,dataVar("isEn",0))).constraints ++
      new GCFilter("isEn","appr1",0,Pred(new Approve,dataVar("isEn",0))).constraints

  val problem1 = baseProblem ++ // init state
    new GCWriter("start",0,List(3)).constraints ++
      new GCWriter("login",0,List(1)).constraints ++
      new GCFifo("start","isEn",None,0).constraints ++
      new GCFifo("start","req1",None,0).constraints ++
      new GCFifo("auth","auth2",None,0).constraints ++
      new GCFifo("pin","appr2",None,0).constraints ++
      GuardedCommands(True --> SGuard(Var(flowVar("start",0)))) ++ // force data on start
      GuardedCommands(True --> SGuard(Var(flowVar("login",0))))    // and on login

  val problem2 = baseProblem ++ // after success of 1
    new GCWriter("start",0,List()).constraints ++
      new GCWriter("login",0,List()).constraints ++
      new GCFifo("start","isEn",Some(3),0).constraints ++
      new GCFifo("start","req1",Some(3),0).constraints ++
      new GCFifo("auth","auth2",Some(1),0).constraints ++
      new GCFifo("pin","appr2",None,0).constraints ++
      GuardedCommands(True --> SGuard(Var(flowVar("pin",0))))    // force data on IMerger

  val problem3 = // if only IMerger had flow before
    new GCWriter("start",0,List()).constraints ++
      new GCWriter("login",0,List()).constraints ++
      new GCFifo("start","isEn",Some(1),0).constraints ++
      new GCFifo("start","req1",None,0).constraints ++
      new GCFifo("auth","auth2",None,0).constraints ++
      new GCFifo("pin","appr2",Some(1),0).constraints ++
      GuardedCommands(True --> SGuard(Var(flowVar("isEn",0)))) // force data before filters


  val time1 = System.currentTimeMillis()
  val res1 = problem1.solve
  val spent1 = System.currentTimeMillis() - time1

  val time2 = System.currentTimeMillis()
  val res2 = problem2.solve
  val spent2 = System.currentTimeMillis() - time2

  val time3 = System.currentTimeMillis()
  val res3 = problem3.solve
  val spent3 = System.currentTimeMillis() - time3

  if (res3.isDefined)  println(res3.get.pretty)

  if (res1.isDefined) println("PAS solved in "+spent1+" ms.")
  else println("PAS - no solution (in "+spent1+" ms)")

  if (res2.isDefined) println("PAS solved in "+spent2+" ms.")
  else println("PAS - no solution (in "+spent2+" ms)")

  if (res3.isDefined) println("PAS solved in "+spent3+" ms.")
  else println("PAS - no solution (in "+spent3+" ms)")

  //    if (res.isDefined) print(spent+" ")
  //    else print("- ")


  ///// PREDICATES ///////

  class Authorised extends Predicate {
    val choPred = (x:IntegerVariable) => Choco.or(Choco.eq(x,1),Choco.eq(x,2))
    val funPred = (x:Int) => (x == 1) || (x == 2)
    override def toString = "Authorised"
  }



  class Deny extends Predicate {
    val choPred = (x:IntegerVariable) => throw new Exception("choPred not implemented")// FIX SOMEHOW!
    val funPred = (x:Int) => salary(x) < (2 * (amount(x) / (period(x)*12)))
    override def toString = "Deny"

    val salary = Map(1 -> 1400, 2 -> 2100, 3 -> 1850) withDefaultValue 2000
    val amount = Map(1 -> 10000, 2 -> 20000, 3 -> 150000) withDefaultValue 1000
    val period = Map(1 -> 150, 2 -> 200, 3 -> 190) withDefaultValue 400
  }

  class Approve extends Predicate {
    val choPred = (x:IntegerVariable) => throw new Exception("choPred not implemented") // FIX SOMEHOW!
    val funPred = (x:Int) => salary(x) > (3 * (amount(x) / (period(x)*12)))
    override def toString = "Approve"

    val salary = Map(1 -> 1400, 2 -> 2100, 3 -> 1850) withDefaultValue 2000
    val amount = Map(1 -> 10000, 2 -> 20000, 3 -> 150000) withDefaultValue 1000
    val period = Map(1 -> 150, 2 -> 200, 3 -> 190) withDefaultValue 400
  }

}
