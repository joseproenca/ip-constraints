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

  private def orSrcs = genSrcOr(srcs)
  private def genSrcOr(lst:List[String]): Guard = lst match {
    case x::Nil => x
    case x::xs  => x or genSrcOr(xs)
    case Nil    => Neg(True)
  }

  private def c1 = snk --> orSrcs

  private def c2 = orSrcs --> snk

  private def c3 = True --> Neg(genSrcAnd(srcs))

  private def genSrcAnd(lst:List[String]): Guard = lst match {
    case Nil    => True
    case x::Nil => Neg(True) // because of the negation before the or - no restrictions if there is 1 end.
    case x1::x2::Nil => x1 and x2
    case x::xs  => x and genSrcAnd(xs)
  }

  private def genData(lst:List[String]): List[GuardedCom] =
    for (src <- srcs) yield src --> (snk := src) //VarAssgn(dataVar(snk,uid),dataVar(src,uid))


  private def constraints = Formula(
    c1,
    c2,
    c3
  )

  private def dataConstraints = constraints ++ genData(srcs)

  if (useCC3) throw new Exception("CC3 not implemented")

//  def getConstraints = constraints
  def getConstraints = if (useData) dataConstraints else constraints

}