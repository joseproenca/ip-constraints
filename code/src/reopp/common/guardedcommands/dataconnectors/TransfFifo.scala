package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common._

class TransfFifo(in: String, out: String, var data: Option[Any], uid: Int = 0)
    extends GCConnector(List(in,out), uid) {

  private val double = Function("double") {
    case i:Int => i*2
    case x     => x.toString
  }
  
  private def emptyFifo = Formula(
    out --> (in /\ (out := (double,in)))
  )

  private def fullFifo = Formula(
    in --> (out /\ (out := (double,in))),
    (!in /\ out) --> (out := data.get)
  )

  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: OptionSol[GCSolution]) = s match  {
    case SomeSol(sol) =>
      if ((sol hasFlowOn mkVar(in)) && !(sol hasFlowOn mkVar(out))) {
        // data goes in
        data = Some(sol(out.dataName))
      }
      else if ((sol hasFlowOn mkVar(out)) && !(sol hasFlowOn mkVar(out))) {
        // data goes out
        data = None
      }
    case _ => {}
  }  	
}
