package partial.reo

import partial.solver.{Neg, Var, Constr}


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 29/02/12
 * Time: 16:43
 * To change this template use File | Settings | File Templates.
 */

abstract class End(name:String, primitive:Primitive) {

    def getName = name

    def getPrimitive = primitive

    def isConnected: Boolean

    def getOther: End

    def getConstraint: Constr

    def getNoFlowConstraint: Constr = Neg(Var(name))
  
    def isReady:Boolean = false

}
