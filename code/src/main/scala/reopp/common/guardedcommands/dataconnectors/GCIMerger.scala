package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands.Neg
import reopp.common.guardedcommands.VarAssgn
import reopp.common.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 19/06/12
 * Time: 21:08
 * To change this template use File | Settings | File Templates.
 */

class GCIMerger(a: String, b: String, c: String) extends GCConnector(List(a,b,c)) {
  private val ab = a+b

  private def constraints = Formula(Set(
    c --> (a or b),
    (a or b) --> c
  ))

  private def dataConstraints =
	  constraints ++ Set(
	    (a and b and ab)      --> VarAssgn(mkDataVar(c),mkDataVar(a)),
	    (a and b and Neg(ab)) --> VarAssgn(mkDataVar(c),mkDataVar(b)),
	    (a and Neg(b)) --> VarAssgn(mkDataVar(c),mkDataVar(a)),
	    (b and Neg(a)) --> VarAssgn(mkDataVar(c),mkDataVar(b))
	  )

  def getConstraints = if (useData) dataConstraints else constraints
}