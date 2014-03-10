package reopp.common.benchmarks

import reopp.common.guardedcommands._
import reopp.common.guardedcommands.dataconnectors.{GCSDrain, GCSSpout, GCFilter}
import reopp.common.{Utils}
import Utils._
import reopp.common.guardedcommands.IntPred
import reopp.common.guardedcommands.Var
import reopp.common.examples.{LT, GT}
import reopp.common.guardedcommands.dataconnectors.GCIFilter

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 21/06/12
 * Time: 14:16
 * To change this template use File | Settings | File Templates.
 */

class GCParSpouts

object GCParSpouts extends App {

  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
          else               2
  val choco = if (args.size > 1) args(1) startsWith "c"
              else               false
  val justInit = if (args.size > 2) args(2) startsWith "i"
                 else               false

  var problem = Formula()

  if (n > 0)
    problem ++=
      ( new GCSSpout("b1","c1").getConstraints ++
        Formula(True --> Var(mkFlowVar("a1"))) ++
        Formula(True --> Var(mkFlowVar("d1"))) ++
        new GCIFilter("b1","a1",new LT(1)).getConstraints ++
        new GCIFilter("c1","d1",new GT(1)).getConstraints
      )

  for (i <- 2 to n) {
    problem ++=
      ( new GCSSpout("b"+i,"c"+i).getConstraints ++
        new GCIFilter("b"+i,"a"+i,new LT(i)).getConstraints ++
        new GCIFilter("c"+i,"d"+i,new GT(i)).getConstraints ++
        new GCSDrain("a"+i,"a"+(i-1)).getConstraints ++
        new GCSDrain("d"+i,"d"+(i-1)).getConstraints
      )
  }

  if (justInit) problem.justInit = true

  if (choco) {
    val time = System.currentTimeMillis()
    val res = problem.solveChocoSat
    val spent = System.currentTimeMillis() - time

    print(spent)

//    if (res.isDefined) println("PAC solved in "+spent+" ms.\n"+res.get.pretty)
//    else println("PAC - no solution (in "+spent+" ms)")
  }
  else {
    val time = System.currentTimeMillis()
    val res = problem.solveChocoDyn
    val spent = System.currentTimeMillis() - time

    print(spent)

//    println(problem.commands.mkString(","))
//    problem.solveDomain()
//    println("fv: "+problem.commands.map(_.fv).foldRight[Set[String]](Set())(_++_))
//    println("da: "+problem.da.pp)
//    println("afv: "+problem.commands.map(_.afv(problem.da)).foldRight[Set[String]](Set())(_++_))

    //    if (res.isDefined) println("PAS solved in "+spent+" ms.\n"+res.get.pretty)
//    else println("PAS - no solution (in "+spent+" ms)")
  }

}
