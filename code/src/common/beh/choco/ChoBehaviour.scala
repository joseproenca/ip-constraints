package common.beh.choco

import common.beh.Behaviour

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:58
 * To change this template use File | Settings | File Templates.
 */

abstract class ChoBehaviour(ends: List[String], uid: Int) extends Behaviour[ChoSolution,ChoConstraints](ends, uid) {
//  def join(c1: ChoConstraints, c2: ChoConstraints) =
//    c1 + c2

  def sync(from:AnyRef,c:ChoConstraints) = {
    if (connections contains from) {
      val connConstr = for ((end,oend,ouid) <- connections(from))
        yield VarEq(Utils.flowVar(oend,ouid),Utils.flowVar(end,uid))
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

  def dataOn(end:String,s:ChoSolution): Any = {
    val data = s.getVal(Utils.dataVar(end,uid))
    if (data.isDefined) data.get
    else 0
  }

  def update(s: ChoSolution) {} // default implementation

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

object ChoBehaviour {
  def apply(ends:List[String],uid:Int, c:ChoConstraints,upd: ChoSolution => Unit): ChoBehaviour = {
    new ChoBehaviour(ends,uid) {
      var constraints = c
      override def update(s:ChoSolution) { upd(s) }

      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
  def apply(ends:List[String],uid:Int, c:ChoConstraints): ChoBehaviour = {
    new ChoBehaviour(ends,uid) {
      var constraints = c
      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
  def apply(ends:List[String],uid:Int, c:ConstrBuilder,upd: ChoSolution => Unit): ChoBehaviour = {
    new ChoBehaviour(ends,uid) {
      var constraints = ChoConstraints(c)
      override def update(s:ChoSolution) { upd(s) }
      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
  def apply(ends:List[String],uid:Int, c:ConstrBuilder): ChoBehaviour = {
    new ChoBehaviour(ends,uid) {
      var constraints = ChoConstraints(c)
      // suggests which ends must have dataflow if "end" has also dataflow
      def guessRequirements(end: String) = Set()
    }
  }
}