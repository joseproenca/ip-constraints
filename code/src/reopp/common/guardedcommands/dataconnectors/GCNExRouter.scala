package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:13
 * To change this template use File | Settings | File Templates.
 */

class GCNExRouter(src: String, snks: List[String], uid: Int) extends GCConnector(src::snks, uid) {
//  def v(x:String) = Var(flowVar(x, uid))


  private def orSnks = genSnkOr(snks)
  private def genSnkOr(lst:List[String]): Guard = lst match {
    case x::Nil => x
    case x::xs  => x or genSnkOr(xs)
    case Nil    => Neg(True)
  }

  private def genSnkAnd(lst:List[String]): Guard = lst match {
    case x::Nil => x
    case x::xs  => x and genSnkAnd(xs)
    case Nil    => True
  }

  private def genData(lst:List[String]): List[GuardedCom] =
    for (snk <- snks) yield snk --> VarAssgn(dataVar(snk,uid),dataVar(src,uid))

  private def c1 = src --> orSnks
  private def c2 = orSnks --> src
  private def c3 = if (snks.tail.isEmpty) True else Neg(genSnkAnd(snks))

  private def constraints = Formula(
    c1,
    c2,
    c3
  )

  private def dataConstraints = constraints ++ genData(snks)

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = if (useData) dataConstraints else constraints

  //  var constraints = Formula(Set(
//    av --> SGuard(bv or cv),
//    (bv or cv) --> SGuard(av),
//    True --> SGuard(Neg(bv and cv)),
//    bv --> VarAssgn(dataVar(b, uid), dataVar(a, uid)),
//    cv --> VarAssgn(dataVar(c, uid), dataVar(a, uid))
//  ))
}