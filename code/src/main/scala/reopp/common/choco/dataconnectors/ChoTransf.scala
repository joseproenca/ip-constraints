package reopp.common.choco.dataconnectors

import reopp.common.choco.connectors
import reopp.common.choco.ChoConstraints
import reopp.common.choco.Var
import reopp.common.Utils._
import reopp.common.choco.VarEq
import choco.kernel.model.variables.integer.IntegerVariable
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import reopp.common.choco.FunAssgn

class ChoTransf(x:String,y:String,uid:Int,f: IntegerVariable => IntegerExpressionVariable) extends connectors.ChoSync(x,y,uid) {

  val (xv,yv) = (Var(flowVar(x,uid)) , Var(flowVar(y,uid)))

  useData = true
  useCC3 = false

  override def getConstraints: ChoConstraints = {
    val c = super.getConstraints
    c impose List(
      xv --> FunAssgn(dataVar(y,uid),dataVar(x,uid),f)
    )
    c
  }  
}