package common.guardedcommands

import common.{CBuilder, Utils, Connector}
import Utils._

/**
 * Connector defined with guarded commands ([[common.guardedcommands.Formula]]).
 * Each concrete connector will extend this class.
 *
 * Created by jose on 06/06/12.
 */
abstract class GCConnector(ends: List[String], uid: Int = 0) extends Connector[GCSolution,Formula](ends,uid) {
  useData = true // data by default


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
      case _ => new ComplexConnector(List(this,other),ends ++ other.ends, uid)
    }



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
  implicit object GCBuilder extends CBuilder[GCSolution,Formula] {
    def sync(e1: String, id1: Int, e2: String, id2: Int): Formula = {
      val sr = Var(flowVar(e1,id1))
      val sk = Var(flowVar(e2,id2))
      st2GC((sr <-> sk) and (sr := sk))
    }
    def noflow(end: String, uid: Int): Formula =
      st2GC(!mkVar(end,uid))
  }
}


