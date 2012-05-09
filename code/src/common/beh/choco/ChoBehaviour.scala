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
//  type mytype = ChoBehaviour

  //  def +(other: Behaviour[ChoConstraints]): Behaviour[ChoConstraints] = {
  //    val res = new ChoBehaviour(ends,uid)
  //    res.constraints = constraints ::: other.constraints
  //    res
  //  }

//  def +(other: ChoBehaviour): ChoBehaviour = {
//    val thisc = constraints
//    // missing a proper handling of primitive uid's, to avoid collisions.
//    new ChoBehaviour(ends ::: other.ends, scala.math.max(uid, other.uid) + 1) {
//      val constraints = thisc + other.constraints
//    }
//  }

//  def join(c1: ChoConstraints, c2: ChoConstraints) =
//    c1 + c2

  def sync(from:AnyRef,c:ChoConstraints) = {
    if (connections contains from) {
      val connConstr = for ((end,oend,ouid) <- connections(from))
      yield VarEq(ConstrBuilder.flowVar(oend,ouid),ConstrBuilder.flowVar(end,uid))
      c ++ connConstr
    }
    else c
  }

  def border(from:AnyRef,c:ChoConstraints) = {
    if (connections contains from) {
      val connConstr = for ((end,_,_) <- connections(from))
        yield Neg(Var(ConstrBuilder.flowVar(end,uid)))
      c ++ connConstr
    }
    else c
  }

  def dataOn(end:String,s:ChoSolution): Any = {
    val data = s.getVal(ConstrBuilder.dataVar(end,uid))
    if (data.isDefined) data.get
    else 0
  }

  def update(s: ChoSolution) {} // default implementation

  def noSol = new ChoSolution(null, null) {
    override def getVal(v: String): Option[Int] =
      extension.get(v)

    override def hasFlow(v: String) = false

    override def pretty: String = {
      //        for (IntegerVariable c:
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
    }
  }
  def apply(ends:List[String],uid:Int, c:ChoConstraints): ChoBehaviour = {
    new ChoBehaviour(ends,uid) {
      var constraints = c
    }
  }
  def apply(ends:List[String],uid:Int, c:ConstrBuilder,upd: ChoSolution => Unit): ChoBehaviour = {
    new ChoBehaviour(ends,uid) {
      var constraints = ChoConstraints(c)
      override def update(s:ChoSolution) { upd(s) }
    }
  }
  def apply(ends:List[String],uid:Int, c:ConstrBuilder): ChoBehaviour = {
    new ChoBehaviour(ends,uid) {
      var constraints = ChoConstraints(c)
    }
  }
}