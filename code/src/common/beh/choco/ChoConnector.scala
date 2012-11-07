package common.beh.choco

import common.beh.{Utils, Connector}


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:58
 * To change this template use File | Settings | File Templates.
 */

abstract class ChoConnector(ends: List[String], uid: Int) extends Connector[ChoSolution,ChoConstraints](ends, uid) {


  /**
   * Combine two connectors, resulting in a composed connector.
   *
   * @param other The other connector to be composed
   * @return The composed connector
   */
  def ++(other: Connector[ChoSolution, ChoConstraints]) = null


  //  def join(c1: ChoConstraints, c2: ChoConstraints) =
//    c1 + c2

  def sync(from:AnyRef,c:ChoConstraints) = {
    if (connections contains from) {
      var connConstr = for ((end,oend,ouid) <- connections(from))
        yield VarEq(Utils.flowVar(oend,ouid),Utils.flowVar(end,uid))
      if (useData) connConstr ++= (for ((end,oend,ouid) <- connections(from))
        yield VarEq(Utils.dataVar(oend,ouid),Utils.dataVar(end,uid)))
      c ++ connConstr
    }
    else c
  }

  def border(from:AnyRef,c:ChoConstraints) = {
    if (connections contains from) {
      val connConstr = for ((end,_,_) <- connections(from))
        yield Neg(Var(Utils.flowVar(end,uid)))
      c ++ connConstr
    }
    else c
  }

  // adds to "c" the flow constraints: at least end must have dataflow
//  def flow(c: ChoConstraints): ChoConstraints = {
//    var endVars = List[String]()
//    for (x <- constraints.Utils.getVars)
//  }

//  def dataOn(end:String,s:ChoSolution): Any = {
//    val data = s.getVal(Utils.dataVar(end,uid))
//    if (data.isDefined) data.get
//    else 0
//  }

  //def update(s: ChoSolution) {} // default implementation

  def noSol = new ChoSolution(null, null) {
    override def getVal(v: String): Option[Int] =
      extension.get(v)

    override def hasFlow(v: String) = false

    override def pretty: String = {
      var res: String = ""
      for (ex <- extension)
        res += ex._1 + " -> " + ex._2
      res
    }
  }
}

object ChoConnector {
  def apply(ends:List[String],uid:Int, c:ChoConstraints,upd: ChoSolution => Unit): ChoConnector = {
    new ChoConnector(ends,uid) {
      def getConstraints = c
      override def update(s:ChoSolution) { upd(s) }

      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
  def apply(ends:List[String],uid:Int, c:ChoConstraints): ChoConnector = {
    new ChoConnector(ends,uid) {
      def getConstraints = c
      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
  def apply(ends:List[String],uid:Int, c:ConstrBuilder,upd: ChoSolution => Unit): ChoConnector = {
    new ChoConnector(ends,uid) {
      def getConstraints = ChoConstraints(c)
      override def update(s:ChoSolution) { upd(s) }
      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
  def apply(ends:List[String],uid:Int, c:ConstrBuilder): ChoConnector = {
    new ChoConnector(ends,uid) {
      def getConstraints = ChoConstraints(c)
      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
}