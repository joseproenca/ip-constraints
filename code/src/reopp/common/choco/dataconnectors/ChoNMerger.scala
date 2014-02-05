package reopp.common.choco.dataconnectors

import reopp.common.choco.connectors
import reopp.common.choco.ChoConnector
import reopp.common.choco.Var
import reopp.common.Utils._
import reopp.common.choco.ChoConstraints
import reopp.common.choco.Neg
import reopp.common.choco.VarEq
import reopp.common.choco.ConstrBuilder
import reopp.common.choco.FalseC
import reopp.common.choco.TrueC

class ChoNMerger(xs:List[String],y:String,uid:Int) extends ChoConnector(y::xs,uid) {

  useData = true
  useCC3 = false

  val xsv = for (x<-xs) yield Var(flowVar(x,uid))
  val yv  = Var(flowVar(y,uid))

  private def ors(xsv:List[ConstrBuilder]): ConstrBuilder = xsv match {
    case Nil => FalseC
    case a::Nil => a
    case a::as => a.or(ors(as))
  } 
  private def ands(xsv:List[ConstrBuilder]): ConstrBuilder = xsv match {
    case Nil => TrueC
    case a::Nil => FalseC // to yield true inside the Neg(ands)
    case a1::a2::Nil => a1 and a2
    case a::as => a.and(ands(as))
  } 
  
  //(z <-> (x1 \/ x2 \/...))  /\ !(x1/\x2/\...) /\  x1 -> ^z1 := ^x1 ... 
  def getConstraints = ChoConstraints(List(
    yv <-> ors(xsv),
    Neg(ands(xsv))) :::
    (for (x <- xs) yield
        Var(flowVar(x,uid)) --> VarEq(dataVar(x,uid),dataVar(y,uid)))
  )
  
 

}