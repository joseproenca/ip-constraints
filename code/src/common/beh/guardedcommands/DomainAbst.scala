package common.beh.guardedcommands

import _root_.choco.kernel.solver.propagation.listener.SetPropagator
import common.beh._
import common.beh.Utils._
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import collection.immutable


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 09:48
 * To change this template use File | Settings | File Templates.
 */

class DomainAbst {
  val greater: Map[String,immutable.Set[String]] = Map() // eg, x -> yz --- y > x, z > x
  val less: Map[String,immutable.Set[String]] = Map()
  val max: Set[String] = Set()
  val inv: Map[String,immutable.Set[UnPredicate]] = Map().withDefaultValue(immutable.Set()) // eg, x -> PQR
  val fun: Map[String,List[UnFunction]] = Map().withDefaultValue(List()) // eg, x -> fgh

  // NOT USED in the end...
//  @deprecated
//  def +=(bigsmall : (String,String)) {
//    val (big,small) = bigsmall
//    // append to a possible bigger
//    if (less contains big)
//      less += big -> (less(big) + small)
//    else
//      less += big -> Set(small)
//    // append to a possible smaller
//    if (greater contains small) {
//      greater += small -> (greater(small) + big)
//    }
//    else
//      greater += small -> Set(big)
//    // update max if no greater than big
//    if (!(greater contains big)) {
//      for (set <- less.get(small); v <- set)
//        max filter (v ==)
//      max += big
//    }
//  }

  // NOT USED in the end...
//  @deprecated
//  def addPred (v: String, pred:IntPredicate) {
//    inv += v -> (inv(v) + pred)
//  }

  /**
   * Calculates the predicates and functions that should be calculated for each end.
   * TODO: Test function.
   * @param x name of the variable representing the end - of shape "F$..."
   * @return a set of predicates that must be evaluated, and a list of functions that must be applied before
   */
  def domainWithEnd(x:String): Set[(UnPredicate,List[UnFunction],String)] = {
    // 1 - start with current predicates
    //    var res = inv(x) map ((_,List[UnFunction]()))
    var res = Set[(UnPredicate,List[UnFunction],String)]()
//    for (p <- inv(x)) res += ((p,fun(x),x))
    for (p <- inv(x)) res += ((p,List(),x))

    if (less contains x) {
      // 2 - add domain of following paths
      for (smaller <- less(x)) {
        if (fun contains smaller)
        // new 3 - add functions to all paths
          res ++= (for ((p,fs,end) <- domainWithEnd(smaller)) yield (p,fs ::: fun(smaller),end))
        else
          res ++= domainWithEnd(smaller)
      }
//      // 3 - add functions to all paths
//      if (fun contains x)
//        res = for ((p,fs,end) <- res) yield (p,fs ::: fun(x), end)
    }
    res
  }


  /**
   * Same as domainMap(x:String) - Calculates the predicates and functions that should be calculated for each end.
   * But it returns a set of predicates and functions with no reference to the
   * TODO: Test function.
   * @param x name of the variable representing the end - of shape "F$..."
   * @return a set of predicates that must be evaluated, and a list of functions that must be applied before
   */
  def domain(x:String): immutable.Set[(UnPredicate,List[UnFunction])] = {
    // 1 - start with current predicates
    var res = inv(x) map ((_,List[UnFunction]()))

    if (less contains x) {
      // 2 - add domain of following paths
      for (smaller <- less(x)) {
        if (fun contains smaller)
          // new 3 - add functions to all paths
          res ++= (for ((p,fs) <- domain(smaller)) yield (p,fs ::: fun(smaller)))
        else
          res ++= domain(smaller)
      }
//      // 3 - add functions to all paths
//      if (fun contains x)
//        res = for ((p,fs) <- res) yield (p,fs ::: fun(x))
    }
    res
  }

  /**
   * Calculates which sequences of functions precede end "x".
   * NOTE: assumption that only source ends of a connector can define data!
   * TODO: test function
   * @param x name of the variable representing the end - of shape "F$..."
   * @return a set of sequences of functions that will be applied
   */
  def pre(x: String): Set[List[UnFunction]] = {
    var res = Set[List[UnFunction]]()
    if (greater contains x)
      for (larger <- greater(x))
        res ++= (for (fs <- pre(larger)) yield fs ::: fun(x))
    else res ++= List()
    res
  }


//  def + (other: DomainAbst): DomainAbst = {
//    val res = new DomainAbst()
//    res.less = less
//    res.greater = greater
//    res.inv = inv
//    res.fun = fun
//
//    for ((k,v) <- other.less)
//      if (res.less contains k) res.less += k -> (res.less(k) ++ v)
//      else res.less += k -> v
//    for ((k,v) <- other.greater)
//      if (res.greater contains k) res.greater += k -> (res.greater(k) ++ v)
//      else res.greater += k -> v
//    for ((k,v) <- other.inv)
//      res.inv += k -> (res.inv(k) ++ v)
//    //      if (res.inv contains k) res.inv += k -> (res.inv(k) ++ v)
//    //      else res.inv += k -> v
//    for ((k,v) <- other.fun)
//      res.fun += k -> (res.fun(k) ::: v)
//
//    res.max = (max ++ other.max) filterNot (res.greater contains _)
//    //    println("--- adding da-fun "+fun+" to "+other.fun+" - result: "+res.fun)
//    res
//  }

//  def ++= (other: DomainAbst) {
//    for ((k,v) <- other.less)
//      if (less contains k) less(k) ++= v
//      else less += k -> v
//    for ((k,v) <- other.greater)
//      if (greater contains k) greater(k) ++= v
//      else greater += k -> v
//    for ((k,v) <- other.inv)
//      inv(k) ++= v
//    for ((k,v) <- other.fun)
//      fun += k -> (fun(k) ::: v)
//
//    max ++= other.max
//    max.filterNot(greater contains _)
//  }

  def += (v:String,p:UnPredicate) {
    inv(v) += p
  }

  def += (v:String,f:UnFunction) {
    fun += v -> (fun(v) ::: List(f))
  }

  def += (bigsmall: (String,String)) {
//    println("adding "+bigsmall+" - domain before:\n"+pp)
    if (less contains bigsmall._1) less(bigsmall._1) += bigsmall._2
    else less += bigsmall._1 -> immutable.Set(bigsmall._2)
    if (greater contains bigsmall._2) greater(bigsmall._2) += bigsmall._1
    else greater += bigsmall._2 -> immutable.Set(bigsmall._1)

//    max += bigsmall._1
//    max.filterNot(greater contains _) // not a mutable operation...
    if (!(greater contains bigsmall._1))
      max += bigsmall._1
    for (s <- less(bigsmall._1)) {
//      println("dropping "+s)
      max -= s
    }

  }


  def guessOrder: List[String] = {
    var done = Set[String]()
    var res = List[String]()
    for (m <- max) {
      val run = addRuns(m,done)
      done = run._1
      res = res ::: run._2
    }
//    println("Ordering variables for the solver: "+res.map(ppFlowVar(_)).mkString(" -> "))
    res
  }

  private def addRuns(start: String, done:Set[String]): (Set[String],List[String]) = {
    var newd = done + start
    var res = if (done contains start) List[String]() else List(start)
    if (less contains  start) for (s <- less(start)) {
      val rest = addRuns(s,newd)
      newd = rest._1
      res = res ::: rest._2
    }
    (newd,res)
  }


  def pp: String = {
    var res = ""
    for ((x,ys) <- greater; y <- ys)
      res += (ppFlowVar(y) + " > " + ppFlowVar(x)+", ")
    if (!greater.isEmpty) res += "\n"
    for ((x,ys) <- less; y <- ys)
      res += (ppFlowVar(y) + " < " + ppFlowVar(x)+", ")
    if (!less.isEmpty) res += "\n"
    for ((v,ps) <- inv)
      res += (ppFlowVar(v) + ": " + ps.mkString(",")+"\n")
    for ((v,fs) <- fun)
      res += (ppFlowVar(v) + ": " + fs.mkString(",")+"\n")
    res += "maxs: "+max.mkString(", ")
    res
  }
}


object DomainAbst {
  def apply(): DomainAbst = new DomainAbst

  def apply(bigsmall : (String,String)): DomainAbst = {
    new DomainAbst() {
      override val greater = Map(bigsmall._2 -> immutable.Set(bigsmall._1))
      override val less = Map(bigsmall._1 -> immutable.Set(bigsmall._2))
      override val max = Set(bigsmall._1)
    }
  }
  def apply(v: String, pred:IntPredicate): DomainAbst = {
    val res = new DomainAbst()
    val p:UnPredicate = pred
    res.inv(v) = immutable.Set(p)
    res
//    {
//      override val inv = Map(v -> Set(pred)).withDefaultValue(Set())
//    }
  }
  def apply(v: String, pred:UnPredicate): DomainAbst = {
    val res = new DomainAbst()
    res.inv(v) = immutable.Set(pred)
    res
    //    new DomainAbst() {
//      override val inv = Map(v -> Set(pred)).withDefaultValue(Set())
//    }
  }
  def apply(v: String, f:UnFunction): DomainAbst = {
    val res = new DomainAbst()
    res.fun(v) = List(f)
    res
    //    new DomainAbst() {
//      override val fun = Map(v -> List(f))
//    }
//    println("--- adding function to domain. New funs: "+res.fun)
  }
}


object OtherMain extends App {
//  val da = new DomainAbst
//  da += "a" -> "c"
//  da += "b" -> "c"
//  da += "c" -> "d"
//  da += "d" -> "e"
//  da += "d" -> "f"
//
//  da addPred ("a",new GT(3))
//  da addPred ("c",new GT(5))
//  da addPred ("e",new Even)
//
////  val da2 = DomainAbst("a" -> "c") + DomainAbst("b" -> "c") + DomainAbst("c" -> "d") +
////            DomainAbst("d" -> "e") + DomainAbst("d" -> "f") +
////            DomainAbst("a",new GT(3)) + DomainAbst("c",new GT(5)) + DomainAbst("e",new Even)
//
//  println(da.pp)
//  for (x <- List("a","b","d","e","f"))
//    println(x+" -- "+da.domain(x).mkString("[",",","]"))
//
////  println(da2.pp)
////  for (x <- List("a","b","d","e","f"))
////    println(x+" -- "+da2.domain(x).mkString("[",",","]"))

}