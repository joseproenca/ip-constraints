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

class GCMerger(a: String, b: String, c: String) extends GCConnector(List(a,b,c)) {
//  val a = Var(flowVar(a,uid))
//  val b = Var(flowVar(b,uid))
//  val c = Var(flowVar(c,uid))

  private def constraints = Formula(
    c --> (a \/ b),
    (a \/ b) --> c,
    !(a and b))

  private def dataConstraints = 
   constraints ++ Set(
    a --> (c := a),
    b --> (c := b))

  def getConstraints = if (useData) dataConstraints else constraints

  if (useCC3) throw new Exception("CC3 not implemented")

}