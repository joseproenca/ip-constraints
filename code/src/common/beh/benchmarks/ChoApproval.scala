package common.beh.benchmarks

import common.beh.choco.dataconnectors._
import common.beh.choco.{Var, ChoConstraints}
import common.beh.Utils._
import choco.kernel.model.variables.integer.IntegerVariable
import choco.Choco

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 21/06/12
 * Time: 13:38
 * To change this template use File | Settings | File Templates.
 */

class ChoApproval

object ChoApproval extends App {

    Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
          else               3

  def genClients(n:Int): Iterable[ChoWriter] = {
    var res = List[ChoWriter]()
    for (i <- 1 to n) {
      res ::=
        new ChoWriter("w"+i,0,List(AllApproval.join(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5)))
      //      println("new writer: "+(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5)+ " -- "+
      //        join(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5))
    }
    res
  }

  def genMergers(height:Int): ChoConstraints = {
    val size = scala.math.pow(2,height)
    var srcs = List("x")
    var res = new ChoConstraints()
    for (level <- 1 to height) {
      var newsrcs = List[String]()
      for (x <- srcs) {
        res = res ++ new ChoMerger(x+1,x+2,x,0).getConstraints
        newsrcs :::= List(x+1,x+2)
      }
      srcs = newsrcs
    }
    for (wr <- genClients(size.toInt)) {
      srcs match {
        case hd::tl =>
          res ++= (wr.getConstraints ++ new ChoSync(wr.x,hd,0).getConstraints)
          srcs = tl
        case Nil => {}
      }
    }
    //    println("res: "+res.commands.mkString(","))
    res
  }

  val approve = new Approve()
  val deny = new Deny()

  val problem = genMergers(n) ++
    //    new ChoExRouter("x","app","y",0).getConstraints ++
    //    new ChoExRouter("y","den","neither",0).getConstraints ++
    new ChoFilter("x","app-ok",0,approve.choPred).getConstraints ++
    new ChoFilter("x","den-ok",0,deny.choPred).getConstraints ++
    new ChoFilter("x","neither-ok",0, (x:IntegerVariable) =>
      Choco.and(Choco.not(approve.choPred(x)),Choco.not(deny.choPred(x)))).getConstraints ++
    ChoConstraints(Var(flowVar("x",0))) // flow on one of the clients

//  println("prob: "+problem.constrBuilders)

  val time = System.currentTimeMillis()
  val res = problem.solve
  val spent = System.currentTimeMillis() - time

  print(spent)

//  if (res.isDefined) println("Choco solved in "+spent+" ms: "+res.get.pretty)
//  else println("Choco - no solution (in "+spent+" ms)")

}