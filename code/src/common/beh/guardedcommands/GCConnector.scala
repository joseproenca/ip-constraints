package common.beh.guardedcommands

import common.beh.{Utils, Connector}
import common.beh.Utils._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:57
 * To change this template use File | Settings | File Templates.
 */

abstract class GCConnector(ends: List[String], uid: Int = 0) extends Connector[GCSolution,GuardedCommands](ends,uid) {
  useData = true // data by default


  /**
   * Combine two connectors, resulting in a composed connector.
   * Note that the uid of the resulting connector is always given
   * by the left ID (or only existing ComplexConnector).
   *
   * @param other The other connector to be composed
   * @return The composed connector
   */
  def ++(other: Connector[GCSolution, GuardedCommands]): Connector[GCSolution,GuardedCommands] =
    (this,other) match {
      case (x:ComplexConnector,_) => x +++ other
      case (_,x:ComplexConnector) => x +++ this
      case _ => new ComplexConnector(List(this,other),ends ++ other.ends, uid)
    }



  /////////////////////////////////////////////////////////////////////////
  // FROM HERE it should probably be dropped and moved to other class... //
  /////////////////////////////////////////////////////////////////////////


  // adds to "c" the sync constraints wrt the ends shared with "from"
  // TODO: fix based on useData or not.
  // NOTE: direction IS important!
  def sync(from: AnyRef, c: GuardedCommands) = {
    if (connections contains from) {
      val glue: Set[GuardedCom]= for ((end,oend,ouid) <- connections(from))
        yield
          { val a = Var(flowVar(end,uid))
            val other = Var(flowVar(oend,ouid))
            st2GC((a <-> other) and (other := a)) }
//          True --> Seq(List(
//          Var(flowVar(oend,ouid)) <-> Var(flowVar(end,uid)),
//          VarAssgn(dataVar(oend,ouid),dataVar(end,uid)) ))
      c ++ glue
    }
    else c
  }

  def dataOn(end: String, s: GCSolution): Any = {
    val data = s.varMap.get(Utils.dataVar(end,uid))
    if (data.isDefined) data.get
    else 0
  }

// adds to "c" the border constraints wrt the ends shared with "from"
//  def border(from: AnyRef, c: GuardedCommands) = null
  def border(from:AnyRef,c:GuardedCommands) = {
    var res = c
    if (connections contains from) {
      val connConstr: Set[GuardedCom] = for ((end,_,_) <- connections(from))
        yield st2GC(!mkVar(end,uid))  //Neg(Var(Utils.flowVar(end,uid)))
      res = c ++ connConstr
    }
    //else c
    println("added borded. New constraints: "+c.commands.mkString(","))
    res
  }

}
