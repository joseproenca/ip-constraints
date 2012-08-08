package common.beh.guardedcommands.dataconnectors

import common.beh.Utils._
import common.beh.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:13
 * To change this template use File | Settings | File Templates.
 */

class GCNExRouter(src: String, snks: List[String], uid: Int) extends GCBehaviour(src::snks, uid) {
  def v(x:String) = Var(flowVar(x, uid))

  val c1 = v(src) --> orSnks

  val orSnks = genSnkOr(snks)
  def genSnkOr(lst:List[String]): Guard = lst match {
    case x::Nil => v(x)
    case x::xs  => v(x) or genSnkOr(xs)
    case Nil    => Neg(True)
  }

  val c2 = orSnks --> v(src)

  val c3 = True --> Neg(genSnkAnd(snks))

  def genSnkAnd(lst:List[String]): Guard = lst match {
    case x::Nil => v(x)
    case x::xs  => v(x) and genSnkAnd(xs)
    case Nil    => True
  }

  def genData(lst:List[String]): List[GuardedCom] =
    for (snk <- snks) yield v(snk) --> VarAssgn(dataVar(snk,uid),dataVar(src,uid))


  var constraints = GuardedCommands(Set(
    c1,
    c2,
    c3
  ))

  if (useData) constraints ++= genData(snks)

  if (useCC3) throw new Exception("CC3 not implemented")

  //  var constraints = GuardedCommands(Set(
//    av --> SGuard(bv or cv),
//    (bv or cv) --> SGuard(av),
//    True --> SGuard(Neg(bv and cv)),
//    bv --> VarAssgn(dataVar(b, uid), dataVar(a, uid)),
//    cv --> VarAssgn(dataVar(c, uid), dataVar(a, uid))
//  ))
}