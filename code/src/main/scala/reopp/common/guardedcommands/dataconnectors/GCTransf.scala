package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.{Function, Utils}
import Utils._
import reopp.common.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/07/12
 * Time: 15:52
 * To change this template use File | Settings | File Templates.
 */

class GCTransf(a: String, b: String, f: Function) extends GCConnector(List(a,b)) {

  private def constraints = Formula(
    a <-> b
  )

  private def dataConstraints = constraints ++
    (a --> (b := (f,a)))  // FunAssgn(dataVar(b,uid), dataVar(a,uid), f)

  if (useCC3) throw new Exception("CC3 not implemented")

  def getConstraints = if (useData) dataConstraints else constraints
}

class GCTTransf[A](a: String, b: String, f: (A) => Any)
    extends GCTransf(a: String, b: String, Function.apply[A]()(f))
