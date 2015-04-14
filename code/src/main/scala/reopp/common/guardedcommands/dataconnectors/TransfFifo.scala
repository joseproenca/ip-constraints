package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common._

class TransfFifo(in: String, out: String, var data: Option[Any])
    extends GCConnector(List(in,out)) {

  private val double = Function("double") {
    case i:Int => i*2
    case x     => x.toString
  }
  
  private def emptyFifo = Formula(
    out --> (in /\ (out := (double,in)))
  )

  private def fullFifo = Formula(
    in --> (out /\ (out := (double,in))),
    (!in /\ out) --> (out :== data.get)
  )

  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: OptionSol[GCSolution]) = s match  {
    case SomeSol(sol) =>
      if ((sol hasFlowOnPort mkVar(in)) && !(sol hasFlowOnPort mkVar(out))) {
        // data goes in
        data = Some(sol(out.data))
      }
      else if ((sol hasFlowOnPort mkVar(out)) && !(sol hasFlowOnPort mkVar(out))) {
        // data goes out
        data = None
      }
    case _ => {}
  }  	
}
