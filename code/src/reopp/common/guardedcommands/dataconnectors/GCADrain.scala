package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.Utils
import Utils._
import reopp.common.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:23
 * To change this template use File | Settings | File Templates.
 */

class GCADrain (a: String, b: String) extends GCConnector(List(a,b)) {


  def getConstraints = if (!useCC3)
	  !(a and b)
    else Formula(
      !(a and b),
      a --> sk(b),
      b --> sr(a),
      (!a /\ !b) --> (!sr(a) /\ !sk(b))
    )
}
