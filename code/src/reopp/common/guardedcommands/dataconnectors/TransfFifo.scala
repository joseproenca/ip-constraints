package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common._

class TransfFifo(ins: String, outs: String, var data: Option[Any], uid: Int = 0)
    extends GCConnector(List(ins,outs), uid) {

  val in  = mkVar(ins)
  val out = mkVar(outs)

  val double = Function("double") {
    case i:Int => i*2
    case x     => x.toString
  }
  
  val emptyFifo = Formula(
    out --> (in /\ (out := (double,in)))
  )

  def fullFifo = Formula(
    in --> (out /\ (out := (double,in))),
    (!in /\ out) --> (out := data.get)
  )

  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: OptionSol[GCSolution]) = s match  {
    case SomeSol(sol) =>
      if ((sol hasFlowOn in) && !(sol hasFlowOn out)) {
        // data goes in
        data = Some(sol(out.dataName))
      }
      else if ((sol hasFlowOn out) && !(sol hasFlowOn out)) {
        // data goes out
        data = None
      }
    case _ => {}
  }  	
}
