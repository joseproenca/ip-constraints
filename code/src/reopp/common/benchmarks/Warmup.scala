package reopp.common.benchmarks

import reopp.common.guardedcommands._
import dataconnectors._
import reopp.common.{Utils, IntPredicate}
import Utils._
import reopp.common.guardedcommands.Neg
import reopp.common.guardedcommands.VarAssgn
import reopp.common.guardedcommands.IntPred
import scala.Some
import reopp.common.guardedcommands.Var
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import reopp.common.IntPredicate

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 20/06/12
 * Time: 09:25
 * To change this template use File | Settings | File Templates.
 */

object Warmup {
  def go {
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


    val morning = new Morning
    val evening  = new Evening

    def genSched(i:Int,on: Boolean): Formula = {
	    val res =
	      new GCExRouter("x"+i,"a"+i,"b"+i).getConstraints ++
	      new GCIFilter("a"+i,"e"+i,evening,positive=false).getConstraints ++
	      new GCIFilter("a"+i,"f"+i,evening).getConstraints ++
	      new GCIFilter("b"+i,"g"+i,morning).getConstraints ++
	      new GCMerger("e"+i,"g"+i,"m"+i).getConstraints ++
	      new GCSDrain("a"+i,"c"+i).getConstraints ++
	      new GCSDrain("b"+i,"d"+i).getConstraints ++
	      new GCSDrain("g"+i,"b"+i).getConstraints ++
	      new GCSync("e"+i,"disp"+i).getConstraints ++
	      new GCSync("f"+i,"off"+i).getConstraints ++
	      new GCSync("g"+i,"on"+i).getConstraints ++
	      Formula(True --> Var(mkFlowVar("x"+i)))
	
	    if (on) res ++
	      new GCSyncFifo("m"+i,"c"+i,Some(Int.box(0))).getConstraints ++
	      new GCFifo("f"+i,"d"+i,None).getConstraints
	    else res ++
	      new GCSyncFifo("m"+i,"c"+i,None).getConstraints ++
	      new GCFifo("f"+i,"d"+i,Some(Int.box(0))).getConstraints
	}
	
	
	def genScheds(uids: Iterable[Int], startVar: String, startUid: Int, on: Boolean): Formula = {
	    var res = Formula()
	    for (i <- uids) {
	      res ++= genSched(i,on)
	      // manual replicator from (startVar.startUid) to (x,i)
	      val av = Var(mkFlowVar(startVar+startUid))
	      val bv = Var(mkFlowVar("x"+i))
	      res ++= Formula(Set(
	        True --> (av <-> bv),
	        av --> VarAssgn(mkDataVar("x"+i), mkDataVar(startVar+startUid))
	      ))
	    }
	    res
	}

    for (n <- 25 to 30) {
      val n2 = n / 2

      val problem = genScheds(1 to n2, "time",0,on = true) ++   // some will display
        genScheds(n2+1 to n, "time",0,on = false) ++            // and some will turn on
        new GCWriter("time0",List(Int.box(500))).getConstraints ++    // (it is morning)
        Formula(True --> Var(mkFlowVar("time0"))) // require some dataflow

      val time = System.currentTimeMillis()
      val res = problem.solveChocoSat
      val spent = System.currentTimeMillis() - time

      val time2 = System.currentTimeMillis()
      val res2 = problem.solveChocoDyn
      val spent2 = System.currentTimeMillis() - time2

//      if (res.isDefined) print(spent+" ")
//      else print("- ")
//      if (res2.isDefined) print(spent2+" ")
//      else print("- ")
//      println("")
    }


  }
}
