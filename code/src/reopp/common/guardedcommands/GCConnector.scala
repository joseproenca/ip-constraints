package reopp.common.guardedcommands

import reopp.common.{CBuilder, Utils, Connector}
import Utils._

/**
 * Connector defined with guarded commands ([[reopp.common.guardedcommands.Formula]]).
 * Each concrete connector will extend this class.
 *
 * Created by jose on 06/06/12.
 */
abstract class GCConnector(ends: List[String]) extends Connector[GCSolution,Formula](ends) {
  useData = true // data by default
  
//  var optID: Option[Int] = None 
  
//  private var id = initID
//
//  def updateID(newID:Int) {
//    id = newID
//  }
//  def getID = id
    

  /**
   * Combine two connectors, resulting in a composed connector.
   * Note that the uid of the resulting connector is always given
   * by the left ID (or only existing ComplexConnector).
   *
   * @param other The other connector to be composed
   * @return The composed connector
   */
  def ++(other: Connector[GCSolution, Formula]): ComplexConnector =
    (this,other) match {
      case (x:ComplexConnector,_) => x +++ other
      case (_,x:ComplexConnector) => x +++ this
      case _ => new ComplexConnector(List(this,other),ends ++ other.ends)
    }

  implicit protected def mkVar(s:String): Var =
//    if (optID.isDefined) Utils.mkVar(Utils.addID(s, optID.get)) else
    Utils.mkVar(s)
  protected def sr(s:String): Var = Var(mkSrcVar(s))
  protected def sk(s:String): Var = Var(mkSnkVar(s))


  /////////////////////////////////////////////////////////////////////////
  // FROM HERE it should probably be dropped and moved to other class... //
  /////////////////////////////////////////////////////////////////////////
//
//
//  // adds to "c" the sync constraints wrt the ends shared with "from"
//  // TODO: fix based on useData or not.
//  // NOTE: direction IS important!
//  def sync(from: AnyRef, c: Formula) = {
//    if (connections contains from) {
//      val glue: Set[GuardedCom]= for ((end,oend,ouid) <- connections(from))
//        yield
//          { val a = Var(flowVar(end,uid))
//            val other = Var(flowVar(oend,ouid))
//            st2GC((a <-> other) and (other := a)) }
////          True --> Seq(List(
////          Var(flowVar(oend,ouid)) <-> Var(flowVar(end,uid)),
////          VarAssgn(dataVar(oend,ouid),dataVar(end,uid)) ))
//      c ++ glue
//    }
//    else c
//  }
//
//  def dataOn(end: String, s: GCSolution): Any = {
//    val data = s.varMap.get(Utils.dataVar(end,uid))
//    if (data.isDefined) data.get
//    else 0
//  }
//
//// adds to "c" the border constraints wrt the ends shared with "from"
////  def border(from: AnyRef, c: Formula) = null
//  def border(from:AnyRef,c:Formula) = {
//    var res = c
//    if (connections contains from) {
//      val connConstr: Set[GuardedCom] = for ((end,_,_) <- connections(from))
//        yield st2GC(!mkVar(end,uid))  //Neg(Var(Utils.flowVar(end,uid)))
//      res = c ++ connConstr
//    }
//    //else c
//    println("added borded. New constraints: "+c.commands.mkString(","))
//    res
//  }

}

object GCConnector {
  /** Needed to carry the sync and noflow operations for GCConnectors. */ 
  implicit object GCBuilder extends CBuilder[GCSolution,Formula] {
    def sync(e1: String, e2: String): Formula = {
      val from = Var(mkFlowVar(e1))
      val to = Var(mkFlowVar(e2))
      st2GC((from <-> to) and (to := from))
    }
    def noflow(end: String): Formula =
      st2GC(!mkVar(end))
  }
}


