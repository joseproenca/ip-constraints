package common.beh.guardedcommands

import collection.mutable.{Set => MutSet}
import collection.{Set => GenSet}
import common.beh.Utils._
import common.beh.choco.{ChoConstraints, FlowPred, ConstrBuilder}
import common.beh.{Solution, UnFunction, IntFunction, IntPredicate}
import choco.kernel.model.variables.integer.IntegerExpressionVariable
import common.beh.choco.genericconstraints.Buffer

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/06/12
 * Time: 12:39
 * To change this template use File | Settings | File Templates.
 */

class PEval(
             var data: Map[String,Any],
             var rest: Map[String,GenSet[String]],
             var funcs: Map[String,GenSet[(String,UnFunction)]]) {

  def ++(other: PEval): PEval = {
    return new PEval(
      data ++ other.data,
      rest ++ (
        for ((x,yset) <- other.rest) yield
          if (rest contains x) x -> (rest(x) ++ yset)
          else x -> yset),
      funcs ++ (
        for ((x,yset) <- other.funcs) yield
          if (funcs contains x) x -> (funcs(x) ++ yset)
          else x -> yset)
    )
  }

  def flattenRest = rest.values.toSet

  /**
   * After the initial partial eval, it simply consumes every element without partitioning.
   * Alternative to quotient
   * @return If all data variables with some data assignment are defined.
   */
  def freshTraversal(buf: Option[Buffer]): Boolean = { //: Map[String,Int] = {
//    println("### init peval: "+this)
    var next = data.keySet
    var continue = true
    while (continue) {
      continue = false
//      println("  ## starting loop - next: "+next.mkString(","))
      while (!next.isEmpty) {
        val hd = next.head
//        println("  # checking head: "+hd)
        if (rest contains hd) {
//          println("  # rest contains head.")
          for (x <- rest(hd)) {
//            println("  # adding data "+data(hd)+" to rest end: "+x)
            data += x -> data(hd)
            next += x
            continue = true
//            rest -= x
          }
//          rest -= hd
        }
        if (buf.isDefined) {
          if (funcs contains hd) for ((y,f) <- funcs(hd)) {
            data += y -> buf.get.calculate(List(f),data(hd))
            next += y
            continue = true
          }
        } else {
          if (funcs contains hd) for ((y,f) <- funcs(hd)) {
            data += y -> f.calculate(data(hd))
            next += y
            continue = true
          }
        }
        next -= hd
      }
    }
//    println("### new peval: "+this)

    // Ending condition: all variables used in sigma-attributions have a data value.
    // Correction: all variables with dataflow have a data value (filtered during partial evaluation)
    var finished = true
    var allvars: Set[String] = Set()
    for ((x,s) <- rest) allvars ++= Set(x) ++ s
    for ((x,s) <- funcs) allvars ++= Set(x) ++ s.map(_._1)
    for (v <- allvars)
      if (!(data contains v)) finished = false

//    println("### finished? "+finished)
    finished
  }





  // ASSUME: instance freshly created with partialEval
  /**
   * Gets a freshly generated PEval and partitions into sets of equal variables, given by the initial 'rest'.
   */
  def quotient() { //: Map[String,MutSet[String]] = {

    var res = Map[String,Wrapper]()

    // for all attributions x := {y}
    for ((x,yset) <- rest; y <- yset) {
      //case 1: x and y have already a group - MERGE
      if ((res contains x) && (res contains y)) {
//        print("## 1 ## ")
        val xl = res(x).leaf
        val yl = res(y).leaf
        if (xl.set != yl.set) {
          xl.set ++= yl.set
          yl.next = Some(xl)
//          for (yv <- res(y).set) {
//            res(x).set.add(yv) // ++= res(y).set
//            res(yv).set = res(x).set
//          }
        }
      }
      //case 2: x has a group, but not y - add y to the group!
      else if (res contains x) {
//        print("## 2 ## ")
        val lf = res(x).leaf
        lf.set.add(y)
        res += y -> lf
      }
      //case 3: y has a group, but not x
      else if (res contains y) {
//        print("## 3 ## ")
        val lf = res(y).leaf
        lf.set.add(x)
        res += x -> lf
      }
      //case 4: new group with "x" and "y"
      else {
//        print("## 4 ## ")
        res += x -> new Wrapper(MutSet[String](x,y),None)
        res += y -> res(x)
      }
//      println("- "+(for ((a,b) <- res) yield b.leaf.set).toSet)
    }

    rest = for ((a,b) <- res) yield a -> b.leaf.set
  }


  // ASSUME: quotient done
  def applyDataAssgn(sol: GCBoolSolution) { //: (Map[String,Int], Set[MutSet[String]]) = { // Map[String,MutSet[String]]) = {

    var next = data.keySet
    var continue = true

    while(continue) {
      continue = false
    // for data vals known by x := int
      for (x <- next) {
        next -= x
        // if x has a group
        if (rest contains x) {
          // go through all elements of that group
          for (v <- rest(x)) {
            // add new data, and drop it from the known groups
            data += v -> data(x)
            next += v
            rest -= v
            rest -= x
            continue = true
          }
        }

        if (funcs contains x) {
          // go through all elements of that group
          for ((v,f) <- funcs(x)) {
            // add new data, and drop it from the known groups
            data += v -> f.calculate(data(x))
            next += v
            funcs -= x
            continue = true
          }
        }
      }
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


  /**
   * Searches for valid inhabitants of the domain of a variable.
   * @param sol
   * @param da
   */
  def solveSimpleData(sol:GCBoolSolution, da: DomainAbst) {
//    println("^^^^ solving simple data copy ^^^^")
//    println(this)

//    var tmprest = rest
//    var delta = delta
    var newdata = Set[String]()

    for (group <- flattenRest) {
      //
      // the group has no data assignment.
      // 3 other options for the group:
      //   (1) depends on data yet undefined,
      //   (2) has some domain restrictions that must be solved, or
      //   (3) has no restrictions -> just give a random value (0)
      //

      var dependent = false
      for ((v,set) <- funcs; (v2,fs) <- set)
        if (group contains v2) dependent = true

      if (!dependent) {
        var genpred = false

//        println("     searching for max vars in group "+group.mkString(","))
        var preds = Set[ConstrBuilder]()
        // Go to all max variables V of this group, and use its domain to build a new predicate P(V) for its inhabitants
        for (v <- group) {
          // TODO: Check if FUNCTIONS need to be considered.
          if (da.max contains v) {
//            println("     max var found: "+v+" - adding predicates from its domain.")
            for ((pred,fs) <- da.domain(v)) {
              // build pred(f1(f2(...(x)))
              def comp(functions: List[IntFunction]): IntegerExpressionVariable => IntegerExpressionVariable  = functions match {
                case Nil => (x:IntegerExpressionVariable) => x
                case (hd::tl) => (x:IntegerExpressionVariable) => hd.choFun(comp(tl)(x))
              }
              // ATTENTION: order of functions was not double-checked.
              if (pred.isInstanceOf[IntPredicate] && fs.isInstanceOf[List[IntFunction]]) {
                val flowpred = FlowPred((x:IntegerExpressionVariable) =>
                  pred.asInstanceOf[IntPredicate].choPred(comp(fs.asInstanceOf[List[IntFunction]])(x)),"x")

                preds += (if (sol(predVar(v,pred,fs))) flowpred
                          else common.beh.choco.Neg(flowpred))
              }
              else
                genpred = true
            }
          }
        }

        if (genpred) {
          // no data can be inferred - group will be negated later (same as no solution)
        }
        // If no such P(V) is found for this group, give value 0 (no constraints)
        else if (preds.isEmpty) {
//          println("     no domain constraints found. Giving '0' to the group.")
          for (v <- group) {
            data += v -> 0
            rest -= v
            newdata += v
          }
        }
        // If some P(V) is found, solve it and include the new assignments
        else {
//          println("     solving new domain constraints using preds "+preds.mkString(" /\\ "))
          val c = new ChoConstraints()
          c impose preds //(for (p <- preds) yield FlowPred(p.choPred,"x"))
          val sol = c.solve
          if (sol.isDefined) {
//            println("     solution for domain! giving "+sol.get.getVal("x")+" to the group.")
            for (v <- group) {
              rest -= v
              data += v -> sol.get.getVal("x").get
              newdata += v
            }
            //                  print(sol.get.pretty)
            //                  println("solved solution for group "+group.mkString("{",",","}"))
          }
//          else
//            println("     no solution! Group needs to be negated later...")


        }
      } // if-end dependend
    }

//    println("     new data added: "+newdata.mkString(","))

    var again = false
    while (!newdata.isEmpty) {
      var newnewdata = Set[String]()
      // if there are functions and new data, functions might be applicable
      for (newd <- newdata)          // for every variable with new data assigned to it
        if (funcs contains newd)
          for ((v,f) <- funcs(newd)) { // with dependent variables v (after applying f)
            var vd = f.calculate(data(newd))
            again = true
            if (rest contains v)
              for (vs <- rest(v)) {
                data += vs -> vd
                rest -= vs
                newnewdata += vs
              }
            else {
              data += v -> vd
              newnewdata += v
            }
          }
      newdata = newnewdata
    }


//    println(this)
//    if (again) println("____ again! ___")
//    else println("____ DONE! ___")

    if (again) solveSimpleData(sol,da)


    //    (delta,rest)
  }


  def isFinished = rest.isEmpty

  def getSol(sol:Solution) : GCSolution = {
    new GCSolution(sol,data)
  }

  override def toString = data.map(x=>ppFlowVar(x._1)+"->"+x._2).mkString(" ,\n")+" /\n"+
    rest.map(x=>ppFlowVar(x._1)+"->"+x._2.map(ppFlowVar(_)).mkString("{",",","}")).mkString(" ;\n")+" /\n"+
    funcs.map(x=>ppFlowVar(x._1)+"->"+x._2.map(y=>ppFlowVar(y._1)+"-"+y._2).mkString("{",",","}")).mkString(" ;\n")

}

private class Wrapper(var set: MutSet[String],var next: Option[Wrapper]) {
  def leaf: Wrapper = if (next.isDefined) next.get.leaf else this
}

