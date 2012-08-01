package common.beh.guardedcommands

import common.beh._
import choco.genericconstraints.{UnFunction, UnPredicate}
import common.beh.Utils._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 07/06/12
 * Time: 09:48
 * To change this template use File | Settings | File Templates.
 */

class DomainAbst {
  var greater: Map[String,Set[String]] = Map() // eg, x -> yz --- y > x, z > x
  var less: Map[String,Set[String]] = Map()
  var max: Set[String] = Set()
  var inv: Map[String,Set[UnPredicate]] = Map().withDefaultValue(Set()) // eg, x -> PQR
  var fun: Map[String,List[UnFunction]] = Map().withDefaultValue(List()) // eg, x -> fgh

  // NOT USED in the end...
  @deprecated
  def +=(bigsmall : (String,String)) {
    val (big,small) = bigsmall
    // append to a possible bigger
    if (less contains big)
      less += big -> (less(big) + small)
    else
      less += big -> Set(small)
    // append to a possible smaller
    if (greater contains small) {
      greater += small -> (greater(small) + big)
    }
    else
      greater += small -> Set(big)
    // update max if no greater than big
    if (!(greater contains big)) {
      for (set <- less.get(small); v <- set)
        max filter (v ==)
      max += big
    }
  }

  // NOT USED in the end...
  @deprecated
  def addPred (v: String, pred:IntPredicate) {
    inv += v -> (inv(v) + pred)
  }

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
    for (p <- inv(x)) res += ((p,fun(x),x))

    if (less contains x) {
      // 2 - add domain of following paths
      for (smaller <- less(x)) {
        //        for ((p,fs) <- domain(smaller))
        //          res(
        res ++= domainWithEnd(smaller)
      }
      // 3 - add functions to all paths
      if (fun contains x)
        res = for ((p,fs,end) <- res) yield (p,fs ::: fun(x), end)
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
  def domain(x:String): Set[(UnPredicate,List[UnFunction])] = {
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

  def + (other: DomainAbst): DomainAbst = {
    val res = new DomainAbst()
    res.less = less
    res.greater = greater
    res.inv = inv
    res.fun = fun

    for ((k,v) <- other.less)
      if (res.less contains k) res.less += k -> (res.less(k) ++ v)
      else res.less += k -> v
    for ((k,v) <- other.greater)
      if (res.greater contains k) res.greater += k -> (res.greater(k) ++ v)
      else res.greater += k -> v
    for ((k,v) <- other.inv)
      res.inv += k -> (res.inv(k) ++ v)
//      if (res.inv contains k) res.inv += k -> (res.inv(k) ++ v)
//      else res.inv += k -> v
    for ((k,v) <- other.fun)
      res.fun += k -> (res.fun(k) ::: v)

    res.max = (max ++ other.max) filterNot (res.greater contains _)
//    println("--- adding da-fun "+fun+" to "+other.fun+" - result: "+res.fun)
    res
  }


  def guessOrder: List[String] = {
    var done = Set[String]()
    var res = List[String]()
    for (m <- max) {
      val run = addRuns(m,done)
      done = run._1
      res = res ::: run._2
    }
    println("Ordering variables for the solver: "+res.map(ppFlowVar(_)).mkString(" -> "))
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
    val res = new DomainAbst()
    res.greater = Map(bigsmall._2 -> Set(bigsmall._1))
    res.less = Map(bigsmall._1 -> Set(bigsmall._2))
    res.max = Set(bigsmall._1)
    res
  }
  def apply(v: String, pred:IntPredicate): DomainAbst = {
    val res = new DomainAbst()
    res.inv = Map(v -> Set(pred))
    res
  }
  def apply(v: String, pred:UnPredicate): DomainAbst = {
    val res = new DomainAbst()
    res.inv = Map(v -> Set(pred))
    res
  }
  def apply(v: String, fun:UnFunction): DomainAbst = {
    val res = new DomainAbst()
    res.fun = Map(v -> List(fun))
//    println("--- adding function to domain. New funs: "+res.fun)
    res
  }
}


object OtherMain extends App {
  val da = new DomainAbst
  da += "a" -> "c"
  da += "b" -> "c"
  da += "c" -> "d"
  da += "d" -> "e"
  da += "d" -> "f"

  da addPred ("a",new GT(3))
  da addPred ("c",new GT(5))
  da addPred ("e",new Even)

  val da2 = DomainAbst("a" -> "c") + DomainAbst("b" -> "c") + DomainAbst("c" -> "d") +
            DomainAbst("d" -> "e") + DomainAbst("d" -> "f") +
            DomainAbst("a",new GT(3)) + DomainAbst("c",new GT(5)) + DomainAbst("e",new Even)

  println(da.pp)
  for (x <- List("a","b","d","e","f"))
    println(x+" -- "+da.domain(x).mkString("[",",","]"))

  println(da2.pp)
  for (x <- List("a","b","d","e","f"))
    println(x+" -- "+da2.domain(x).mkString("[",",","]"))

}