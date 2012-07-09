package common.beh.benchmarks

import common.beh.guardedcommands._
import common.beh.guardedcommands.dataconnectors._
import scala.math.pow
import common.beh.Predicate
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import choco.Choco
import common.beh.Utils._
import common.beh.guardedcommands.Pred
import common.beh.guardedcommands.SGuard

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 21/06/12
 * Time: 10:25
 * To change this template use File | Settings | File Templates.
 */

class GCApproval

object GCApproval extends App {

  Warmup.go

  val n = if (!args.isEmpty) Integer.parseInt(args(0))
          else               4
  val choco = if (args.size > 1) args(1) startsWith "c"
              else               false
  val justInit = if (args.size > 2) args(2) startsWith "i"
                 else               false


  def join(id:Int, f1:Int, f2:Int, f3:Int) =
    id*21*21*21 + f1*21*21 + f2*21 + f3

  def split(joined:Int) = {
    val f3 = joined % 21
    val f2 = ((joined - f3) / 21 ) % 21
    val f1 = ((joined - f3 - f2*21) / 441 ) % 21
    val id = ((joined - f3 - f2*21 - f1*441) / (9261) ) % 21
    (id,f1,f2,f3)
  }

  def choF3(n:IntegerExpressionVariable) = Choco.mod(n,21)

  def choF2(n:IntegerExpressionVariable) =
    Choco.mod(Choco.div(Choco.minus(n,choF3(n)),21),21)

  def choF1(n:IntegerExpressionVariable) =
    Choco.mod(Choco.div(Choco.minus(Choco.minus(n,choF3(n)),Choco.mult(choF2(n),21)),441),21)


  def genClients(n:Int): Iterable[GCWriter] = {
    var res = List[GCWriter]()
    for (i <- 1 to n) {
      res ::=
        new GCWriter("w"+i,0,List(join(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5)))
//      println("new writer: "+(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5)+ " -- "+
//        join(i,(i*3 % 16)+5,(i*4 % 16)+5,(i*5 % 16)+5))
    }
    res
  }

  def genMergers(height:Int): GuardedCommands= {
    val size = pow(2,height)
    var srcs = List("x")
    var res = GuardedCommands()
    for (level <- 1 to height) {
      var newsrcs = List[String]()
      for (x <- srcs) {
        res = res ++ new GCMerger(x+1,x+2,x,0).constraints
        newsrcs :::= List(x+1,x+2)
      }
      srcs = newsrcs
    }
//    println("size / n.of srcs: "+size+"/"+srcs.size)
//    println("clients: "+genClients(size.toInt).map(_.x))
    res
  }


  def genMergers2(size:Int): GuardedCommands= {
      val height = scala.math.log(size) / scala.math.log(2)
      var srcs = List("x")
      var res = GuardedCommands()
      for (level <- 1 to height.toInt) {
        var newsrcs = List[String]()
        for (x <- srcs) {
          res = res ++ new GCMerger(x+1,x+2,x,0).constraints
          newsrcs :::= List(x+1,x+2)
        }
        srcs = newsrcs
      }


      for (wr <- genClients(size.toInt)) {
        srcs match {
          case hd::tl =>
            res ++= (wr.constraints ++ new GCSync(wr.x,hd,0).constraints)
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
//    new GCExRouter("x","app","y",0).constraints ++
//    new GCExRouter("y","den","neither",0).constraints ++
    new GCFilter("x","app-ok",0,Pred(dataVar("x",0),approve)).constraints ++
    new GCFilter("x","den-ok",0,Pred(dataVar("x",0),deny)).constraints ++
    new GCFilter("x","neither-ok",0,
      Neg(Pred(dataVar("x",0),approve)) and Neg(Pred(dataVar("x",0),deny))).constraints ++
    GuardedCommands(True --> SGuard(Var(flowVar("x",0)))) // flow on one of the clients

  if (justInit) problem.justInit = true


  if (choco) {
    val time = System.currentTimeMillis()
    val res = problem.solveChocoSat
    val spent = System.currentTimeMillis() - time

    print(spent)

//    if (res.isDefined) println("PAC solved in "+spent+" ms: "+res.get.pretty)
//    else println("PAC - no solution (in "+spent+" ms)")
  }
  else {
    val time = System.currentTimeMillis()
    val res = problem.solve
    val spent = System.currentTimeMillis() - time

    print(spent)

//    if (res.isDefined) println("PAS solved in "+spent+" ms:\n"+res.get.pretty)
//    else println("PAS - no solution (in "+spent+" ms)")
  }



  //  val p2 =
//    new GCFilter("app","app-ok",0,Pred(new Approve(),dataVar("app",0))).constraints ++
//    GuardedCommands(True --> SGuard(Var(flowVar("app-ok",0))))

//  val res = p2.solve
//  if (res.isDefined) println("res: "+res.get.pretty)
//  else println("no sol")
}



/// PREDICATES

class Approve extends Predicate {
  val choPred = (x:IntegerExpressionVariable) =>
    Choco.leq(140,Choco.sum(
      Choco.mult(GCApproval.choF1(x),2),
      Choco.mult(GCApproval.choF2(x),3),
      Choco.mult(GCApproval.choF3(x),5)
    ))

  val funPred = (x:Int) => {
    val v = GCApproval.split(x)
    (v._2*2 + v._3*3 + v._4*5) >= 140
  }

  override def toString = "Approve"
}

class Deny extends Predicate {
  val choPred = (x:IntegerExpressionVariable) =>
    Choco.geq(90,Choco.sum(
      Choco.mult(GCApproval.choF1(x),2),
      Choco.mult(GCApproval.choF2(x),3),
      Choco.mult(GCApproval.choF3(x),5)
    ))
  val funPred = (x:Int) => {
    val v = GCApproval.split(x)
    (v._2*2 + v._3*3 + v._4*5) <= 90
  }

  override def toString = "Deny"
}


