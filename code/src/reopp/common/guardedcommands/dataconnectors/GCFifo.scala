package reopp.common.guardedcommands.dataconnectors

import reopp.common.{OptionSol, Utils}
import Utils._
import reopp.common.guardedcommands._
import scala.None
import reopp.common.SomeSol

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 11/06/12
 * Time: 14:43
 * To change this template use File | Settings | File Templates.
 */

class GCFifo(a: String, b: String, var data: Option[Any]) extends GCConnector(List(a,b)) {

//  def this(a: String, b: String, dt: Option[Int], uid: Int) = this(a,b, dt.map(Int.box(_)),uid)
  def this(a: String, b: String) = this(a,b, scala.None:Option[Any])

  private def emptyFifo = Formula(!b)

  private def fullFifo: Formula =
    if (useData) Formula(
        !a,
        b --> (b :== data.get)
      )
    else if (useCC3) throw new Exception("CC3 not implemented")
    else !a //Formula(Neg(a))


  def getConstraints = if (data.isDefined) fullFifo else emptyFifo

  override def update(s: OptionSol[GCSolution]) = s match  {
    case SomeSol(sol) =>
      if (sol hasFlowOn a.flow) {
        data = Some(sol(a.data))
        // println("FIFO: FLOW IN!")
      }
      else if (sol hasFlowOn b.flow) {
        data = None
        // println("FIFO: FLOW OUT!")
      }
    case _ => {}
  }  	
}
