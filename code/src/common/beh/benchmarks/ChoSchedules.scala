package common.beh.benchmarks

import common.beh.choco.dataconnectors._
import choco.kernel.model.variables.integer.{IntegerExpressionVariable, IntegerVariable}
import choco.Choco
import common.beh.Utils._
import common.beh.choco.{VarEq, Var, ChoConstraints}
import common.beh.Predicate

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 18:31
 * To change this template use File | Settings | File Templates.
 */

class ChoSchedules

object ChoSchedules extends App {

  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
          else               2


  def genSched(i:Int,on: Boolean): ChoConstraints = {
    val morning = new Morning
    val evening  = new Evening

    //    new ChoWriter("x",i,List(500)).constraints ++
    val res =
      new ChoExRouter("x","a","b",i).constraints ++
        new ChoFilter("a","e",i, (x:IntegerVariable) => Choco.not(evening.choPred(x))).constraints ++
         //  Neg(Pred(evening,dataVar("a",i)))).constraints ++
        new ChoFilter("a","f",i,evening.choPred).constraints ++
        new ChoFilter("b","g",i,morning.choPred).constraints ++
        new ChoMerger("e","g","m",i).constraints ++
        new ChoSDrain("a","c",i).constraints ++
        new ChoSDrain("b","d",i).constraints ++
        new ChoSDrain("g","b",i).constraints ++
        new ChoSync("e","disp",i).constraints ++
        new ChoSync("f","off",i).constraints ++
        new ChoSync("g","on",i).constraints ++
        ChoConstraints(Var(flowVar("x",i)))

    if (on) res ++
      new ChoSyncFifo("m","c",Some(0),i).constraints ++
      new ChoFifo("f","d",None,i).constraints
    else res ++
      new ChoSyncFifo("m","c",None,i).constraints ++
      new ChoFifo("f","d",Some(0),i).constraints
  }


  def genScheds(uids: Iterable[Int], startVar: String, startUid: Int, on: Boolean): ChoConstraints= {
    var res = new ChoConstraints()
    for (i <- uids) {
      res ++= genSched(i,on)
      // manual replicator from (startVar.startUid) to (x,i)
      val av = Var(flowVar(startVar,startUid))
      val bv = Var(flowVar("x",i))
      res ++= ChoConstraints(List(
        av <-> bv,
        VarEq(dataVar("x",i), dataVar(startVar,startUid))
      ))
    }
    res
  }


//  val problem = genSched(0,true)  ++ new ChoWriter("x",0,List(500)).constraints  // on, morning  - display
//    val schedule = genSched(0,true)  ++ new ChoWriter("x",0,List(1400)).constraints // on, evening  - turn off
//    val schedule = genSched(0,false) ++ new ChoWriter("x",0,List(500)).constraints  // off, morning - turn on
//    val schedule = genSched(0,false) ++ new ChoWriter("x",0,List(1400)).constraints // off, evening - no sol

  val n2: Int = n / 2
  val problem = genScheds(1 to n2, "time",0,true) ++ // some will display
    genScheds(n2+1 to n, "time",0,false) ++            // and some will turn on
    new ChoWriter("time",0,List(500)).constraints       // (it is morning)


  val time = System.currentTimeMillis()
  val res = problem.solve
  val spent = System.currentTimeMillis() - time

  print(spent)

  //  if (res.isDefined) println("CHOCO solved in "+spent+" ms. - "+n+"/"+n2)
//  else println("no solution (in "+spent+" ms)")

//  if (res.isDefined) println(res.get.pretty)
//  if (res.isDefined) println("partial eval: "+schedule.partialEval(res.get))


  class Morning extends Predicate {
    // from 7am to 10am
    val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,420),Choco.leq(x,600))
    val funPred = (x:Int) => (x >= 40) && (x <= 600)
    override def toString = "Morning"
  }
  class Evening extends Predicate {
    // from 7pm to midnight
    val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,1140),Choco.leq(x,1440))
    val funPred = (x:Int) => (x >= 1140) && (x <= 1440)
    override def toString = "Evening"
  }
}

