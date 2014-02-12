package partial.solver

import choco.kernel.model.constraints.Constraint
import choco.kernel.model.variables.integer.IntegerVariable
import choco.Choco

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 29/02/12
 * Time: 16:49
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class Constr {

  type VarMap = Map[String,IntegerVariable]

  def toChoco : Constraint = toChoco(Map())._2

  def toChoco(vars:VarMap): (VarMap,Constraint) = this match {
    case Var(name:String) => {
      val (m,v) = Constr.getVar(vars,name)
      (m, Choco.eq(v,1))
    }
    case Neg(c:Constr) => (vars,Choco.not(c.toChoco(vars)_2))
    case And(c1:Constr,c2:Constr) => {
      val (m1,v1) = c1.toChoco(vars)
      val (m2,v2) = c2.toChoco(m1)
      (m2, Choco.and(v1,v2))
    }
    case Or(c1:Constr,c2:Constr) => {
      val (m1,v1) = c1.toChoco(vars)
      val (m2,v2) = c2.toChoco(m1)
      (m2, Choco.or(v1,v2))
    }
    // TODO: complete cases
    case _ => (vars,Choco.TRUE)
  }

}

object Constr {
  type VarMap = Map[String,IntegerVariable]

  def getVar(m:VarMap,name:String): (VarMap,IntegerVariable) = {
    if (m contains name)
      (m,m(name))
    else {
      val v = Choco.makeBooleanVar(name)
      (m+(name -> v), v)
    }
  }
}
//    IntegerVariable v;
//    if (m.containsKey(s)) v = m.get(s);
//    else {
//      v = Choco.makeBooleanVar(s);
//      m.put(s, v);
//    }
//    return v;
//  }

case class Var(name:String)  extends Constr
case class Neg(c:Constr) extends Constr
case class And(c1:Constr,c2:Constr)   extends Constr
case class Or(c1:Constr,c2:Constr)    extends Constr
case class Impl(c1:Constr,c2:Constr)  extends Constr
case class Equiv(c1:Constr,c2:Constr) extends Constr