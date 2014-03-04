package reopp.common.choco

import reopp.common.{CBuilder, Connector}
import reopp.common.Utils._


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:58
 * To change this template use File | Settings | File Templates.
 */

abstract class ChoConnector(ends: List[String], uid: Int) extends Connector[ChoSolution,ChoConstraints](ends) {


  /**
   * Combine two connectors, resulting in a composed connector.
   *
   * @param other The other connector to be composed
   * @return The composed connector
   */
  def ++(other: Connector[ChoSolution, ChoConstraints]) = {
    val mycs = getConstraints
    new ChoConnector(ends ::: other.ends, uid){
      def getConstraints =  mycs ++ other.getConstraints
    }
    //throw new RuntimeException("Composition of choco-based connectors not defined.")
  }

  throw new RuntimeException("ChoConnector not maintained.\n"+
      "Only works when IDs are alwyas constant - not the case with nodes (workers).\n"+
      "Need to make IDs of variable dynamic.")
  
  def getID = uid
  def updateID(newID:Int) = {}

    //
//
//  //  def join(c1: ChoConstraints, c2: ChoConstraints) =
////    c1 + c2
//
//  def sync(from:AnyRef,c:ChoConstraints) = {
//    if (connections contains from) {
//      var connConstr = for ((end,oend,ouid) <- connections(from))
//        yield VarEq(Utils.flowVar(oend,ouid),Utils.flowVar(end,uid))
//      if (useData) connConstr ++= (for ((end,oend,ouid) <- connections(from))
//        yield VarEq(Utils.dataVar(oend,ouid),Utils.dataVar(end,uid)))
//      c ++ connConstr
//    }
//    else c
//  }
//
//  def border(from:AnyRef,c:ChoConstraints) = {
//    if (connections contains from) {
//      val connConstr = for ((end,_,_) <- connections(from))
//        yield Neg(Var(Utils.flowVar(end,uid)))
//      c ++ connConstr
//    }
//    else c
//  }

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

  def noSol = new ChoSolution(null, null, None) {
    override def getVal(v: String): Option[Int] =
      extension.get(v)

    override def hasFlowOn(v: String) = false

    override def toString: String = {
      var res: String = ""
      for (ex <- extension)
        res += ex._1 + " -> " + ex._2
      res
    }
  }
}

object ChoConnector {
//  def apply(ends:List[String],uid:Int, c:ChoConstraints,upd: ChoSolution => Unit): ChoConnector = {
//    new ChoConnector(ends,uid) {
//      def getConstraints = c
//      override def update(s:Option[ChoSolution]) { upd(s) }
//
//      // suggests which ends must have dataflow if "end" has also dataflow
//      def guessRequirements(end: String) = Set()
//    }
//  }
  def apply(ends:List[String],uid:Int, c:ChoConstraints): ChoConnector = {
    new ChoConnector(ends,uid) {
      def getConstraints = c
      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
//  def apply(ends:List[String],uid:Int, c:ConstrBuilder,upd: ChoSolution => Unit): ChoConnector = {
//    new ChoConnector(ends,uid) {
//      def getConstraints = ChoConstraints(c)
//      override def update(s:Option[ChoSolution]) { upd(s) }
//      // suggests which ends must have dataflow if "end" has also dataflow
//      def guessRequirements(end: String) = Set()
//    }
//  }
  def apply(ends:List[String],uid:Int, c:ConstrBuilder): ChoConnector = {
    new ChoConnector(ends,uid) {
      def getConstraints = ChoConstraints(c)
      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }

  implicit object ChoBuilder extends CBuilder[ChoSolution,ChoConstraints] {
    def sync(e1: String, id1: Int, e2: String, id2: Int): ChoConstraints = {
      var res: ConstrBuilder = VarEq(flowVar(e1,id1),flowVar(e2,id2))
//      if (useData)
      res = And(res, VarEq(dataVar(e1,id1),dataVar(e2,id2)))
      //res.toChoco()
      //throw new RuntimeException("sync method not implemented for choco constraints.")
      ChoConstraints(res)
    }
    def noflow(end: String, uid: Int): ChoConstraints =
      ChoConstraints(Neg(Var(flowVar(end,uid))))
  }


}