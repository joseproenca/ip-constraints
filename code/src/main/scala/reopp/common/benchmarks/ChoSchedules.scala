package reopp.common.benchmarks

import reopp.common.choco.dataconnectors._
import choco.kernel.model.variables.integer.{IntegerExpressionVariable, IntegerVariable}
import choco.Choco
import reopp.common.{Utils, IntPredicate}
import Utils._
import reopp.common.choco.{VarEq, Var, ChoConstraints}
import reopp.common.IntPredicate

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
      new ChoExRouter("x","a","b",i).getConstraints ++
        new ChoFilter("a","e",i, (x:IntegerVariable) => Choco.not(evening.choPred(x))).getConstraints ++
         //  Neg(Pred(evening,dataVar("a",i)))).getConstraints ++
        new ChoFilter("a","f",i,evening.choPred).getConstraints ++
        new ChoFilter("b","g",i,morning.choPred).getConstraints ++
        new ChoMerger("e","g","m",i).getConstraints ++
        new ChoSDrain("a","c",i).getConstraints ++
        new ChoSDrain("b","d",i).getConstraints ++
        new ChoSDrain("g","b",i).getConstraints ++
        new ChoSync("e","disp",i).getConstraints ++
        new ChoSync("f","off",i).getConstraints ++
        new ChoSync("g","on",i).getConstraints ++
        ChoConstraints(Var(mkFlowVar("x")))

    if (on) res ++
      new ChoSyncFifo("m","c",Some(0),i).getConstraints ++
      new ChoFifo("f","d",None,i).getConstraints
    else res ++
      new ChoSyncFifo("m","c",None,i).getConstraints ++
      new ChoFifo("f","d",Some(0),i).getConstraints
  }


  def genScheds(uids: Iterable[Int], startVar: String, startUid: Int, on: Boolean): ChoConstraints= {
    var res = new ChoConstraints()
    for (i <- uids) {
      res ++= genSched(i,on)
      // manual replicator from (startVar.startUid) to (x,i)
      val av = Var(mkFlowVar(startVar))//,startUid))
      val bv = Var(mkFlowVar("x"))
      res ++= ChoConstraints(List(
        av <-> bv,
        VarEq(mkDataVar("x"), mkDataVar(startVar))//,startUid))
      ))
    }
    res
  }


//  val problem = genSched(0,true)  ++ new ChoWriter("x",0,List(500)).getConstraints  // on, morning  - display
//    val schedule = genSched(0,true)  ++ new ChoWriter("x",0,List(1400)).getConstraints // on, evening  - turn off
//    val schedule = genSched(0,false) ++ new ChoWriter("x",0,List(500)).getConstraints  // off, morning - turn on
//    val schedule = genSched(0,false) ++ new ChoWriter("x",0,List(1400)).getConstraints // off, evening - no sol

  val n2: Int = n / 2
  val problem = genScheds(1 to n2, "time",0,on = true) ++ // some will display
    genScheds(n2+1 to n, "time",0,on = false) ++            // and some will turn on
    new ChoWriter("time",0,List(500)).getConstraints       // (it is morning)


  val time = System.currentTimeMillis()
  val res = problem.solve()
  val spent = System.currentTimeMillis() - time

  print(spent)

  //  if (res.isDefined) println("CHOCO solved in "+spent+" ms. - "+n+"/"+n2)
//  else println("no solution (in "+spent+" ms)")

//  if (res.isDefined) println(res.get.pretty)
//  if (res.isDefined) println("partial eval: "+schedule.partialEval(res.get))


  class Morning extends IntPredicate {
    // from 7am to 10am
    val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,420),Choco.leq(x,600))
    val funPred = (x:Int) => (x >= 40) && (x <= 600)
    override def toString = "Morning"

    val z3Pred = null
  }
  class Evening extends IntPredicate {
    // from 7pm to midnight
    val choPred = (x:IntegerExpressionVariable) => Choco.and(Choco.geq(x,1140),Choco.leq(x,1440))
    val funPred = (x:Int) => (x >= 1140) && (x <= 1440)
    override def toString = "Evening"

    val z3Pred = null
  }
}

