package reopp.common.guardedcommands.dataconnectors

import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:21
 * To change this template use File | Settings | File Templates.
 */

class GCNMerger(srcs: List[String], snk: String, uid: Int) extends GCConnector(snk :: srcs, uid) {
  def v(x:String) = Var(flowVar(x, uid))

  val orSrcs = genSrcOr(srcs)
  def genSrcOr(lst:List[String]): Guard = lst match {
    case x::Nil => v(x)
    case x::xs  => v(x) or genSrcOr(xs)
    case Nil    => Neg(True)
  }

  val c1 = v(snk) --> orSrcs

  val c2 = orSrcs --> v(snk)

  val c3 = True --> Neg(genSrcAnd(srcs))

  def genSrcAnd(lst:List[String]): Guard = lst match {
    case Nil    => True
    case x::Nil => Neg(True) // because of the negation before the or - no restrictions if there is 1 end.
    case x1::x2::Nil => v(x1) and v(x2)
    case x::xs  => v(x) and genSrcAnd(xs)
  }

  def genData(lst:List[String]): List[GuardedCom] =
    for (src <- srcs) yield v(src) --> (v(snk) := v(src)) //VarAssgn(dataVar(snk,uid),dataVar(src,uid))


  var constraints = Formula(
    c1,
    c2,
    c3
  )

  if (useData) constraints ++= genData(srcs)

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = constraints
}