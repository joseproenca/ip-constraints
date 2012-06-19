package common.beh.guardedcommands

import collection.mutable.{Set => MutSet}
import collection.{Set => GenSet}
import common.beh.Utils._
import common.beh.choco.{ChoConstraints, FlowPred, ConstrBuilder}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/06/12
 * Time: 12:39
 * To change this template use File | Settings | File Templates.
 */

class PEval(var data: Map[String,Int], var rest: Map[String,GenSet[String]]) {

  def ++(other: PEval): PEval = {
    return new PEval(data ++ other.data, rest ++
      ( for ((x,yset) <- other.rest) yield
          if (rest contains x) x -> (rest(x) ++ yset)
          else x -> yset
      ))
  }

  def flattenRest = rest.values.toSet

  // ASSUME: instance freshly created with partialEval
  def quotient() { //: Map[String,MutSet[String]] = {

    class Wrapper(var set: MutSet[String])


    var res = Map[String,Wrapper]()

    // for all attributions x := {y}
    for ((x,yset) <- rest; y <- yset) {
      //case 1: x and y have already a group - MERGE
      if ((res contains x) && (res contains y)) {
//        print("## 1 ## ")
        if (res(x).set != res(y).set) {
          for (yv <- res(y).set) {
            res(x).set.add(yv) // ++= res(y).set
            res(yv).set = res(x).set
          }
        }
      }
      //case 2: x has a group, but not y - add y to the group!
      else if (res contains x) {
//        print("## 2 ## ")
        res(x).set.add(y)
        res += y -> res(x)
      }
      //case 3: y has a group, but not x
      else if (res contains y) {
//        print("## 3 ## ")
        res(y).set.add(x)
        res += x -> res(y)
      }
      //case 4: new group with "x" and "y"
      else {
//        print("## 4 ## ")
        res += x -> new Wrapper(MutSet[String](x,y))
        res += y -> res(x)
      }
//      println("- "+(for ((a,b) <- res) yield b.set).toSet)
    }

    rest = for ((a,b) <- res) yield a -> b.set
  }


  // ASSUME: quotient done
  def applyDataAssgn(sol: GCBoolSolution) { //: (Map[String,Int], Set[MutSet[String]]) = { // Map[String,MutSet[String]]) = {
    //val peval = partialEval(sol)
    //var partition = peval.quotient() //quotient(peval)
//    var res = Map[String,Int]()

    // for data vals known by x := int
    for ((x,int) <- data) {
      // if x has a group
      if (rest contains x) {
        // go through all elements of that group
        for (v <- rest(x)) {
          // add new data, and drop it from the known groups
          data += v -> int
          rest -= v
        }
      }
      // if x has no group
      else
        data += x -> int
    }

    // extend partition with variables with dataflow, not in the leftovers of the partition
    // and without known data yet
    for ((v,bool) <- sol.varMap)
      if (isFlowVar(v) && bool) {
        val vd = flow2data(v)
        if (!(data contains vd) && !(rest contains vd))
          rest += vd -> Set(vd)
      }
  }


  def solveSimpleData(sol:GCBoolSolution, da: DomainAbst) {
//    var tmprest = rest
//    var delta = delta
    for (group <- flattenRest) {
      var preds = Set[ConstrBuilder]()
      for (v <- group) {
        if (da.max contains v)
          for (pred <- da.domain(v)) {
            val pvar = predVar(v,pred)
            preds += (if (sol(pvar)) FlowPred(pred.choPred,"x")
            else common.beh.choco.Neg(FlowPred(pred.choPred,"x")))
          }
      }
      //      preds ++= da.domain(v)
//      println("solving group "+group.mkString("{",",","}")+
//        " by adding to predicates "+preds.mkString("{",",","}"))
      if (preds.isEmpty) {
        for (v <- group) {
          data += v -> 0
          rest -= v
        }
      }
      else {
        val c = new ChoConstraints()
        c impose preds //(for (p <- preds) yield FlowPred(p.choPred,"x"))
        val sol = c.solve
        if (sol.isDefined) {
          for (v <- group) {
            rest -= v
            data += v -> sol.get.getVal("x").get
          }
          //                  print(sol.get.pretty)
          //                  println("solved solution for group "+group.mkString("{",",","}"))
        }

      }
    }
//    (delta,rest)
  }


  def isFinished = rest.isEmpty

  def getSol(sol:GCBoolSolution) : GCSolution = {
    new GCSolution(sol,data)
  }

  override def toString = (data,rest).toString()
}


