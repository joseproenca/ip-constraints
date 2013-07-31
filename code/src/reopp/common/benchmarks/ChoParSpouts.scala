package reopp.common.benchmarks

import reopp.common.choco.{Var, ChoConstraints}
import reopp.common.choco.dataconnectors._
import reopp.common.{Utils}
import Utils._
import reopp.common.examples.{LT, GT}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 21/06/12
 * Time: 14:30
 * To change this template use File | Settings | File Templates.
 */

class ChoParSpouts

object ChoParSpouts extends App {

  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
  else               100
  val choco = if (args.size > 1) args(1) startsWith "c"
  else               false
  val justInit = if (args.size > 2) args(2) startsWith "i"
  else               false

  var problem = new ChoConstraints()

  if (n > 0)
    problem ++=
      ( new ChoSSpout("b1","c1",0).getConstraints ++
        ChoConstraints(Var(flowVar("a1",0))) ++
        ChoConstraints(Var(flowVar("d1",0))) ++
        new ChoFilter("b1","a1",0,new LT(1).choPred).getConstraints ++
        new ChoFilter("c1","d1",0,new GT(1).choPred).getConstraints
        )

  for (i <- 2 to n) {
    problem ++=
      ( new ChoSSpout("b"+i,"c"+i,0).getConstraints ++
        new ChoFilter("b"+i,"a"+i,0,new LT(i).choPred).getConstraints ++
        new ChoFilter("c"+i,"d"+i,0,new GT(i).choPred).getConstraints ++
        new ChoSDrain("a"+i,"a"+(i-1),0).getConstraints ++
        new ChoSDrain("d"+i,"d"+(i-1),0).getConstraints
        )
  }

  val time = System.currentTimeMillis()
  val res = problem.solve
  val spent = System.currentTimeMillis() - time

  print(spent)

//  if (res.isDefined) println("Choco solved in "+spent+" ms.\n"+res.get.pretty)
//  else println("Choco - no solution (in "+spent+" ms)")

}
