package common.beh.guardedcommands

import common.beh.choco.dataconnectors.{Predicate,Even,GT}


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
  var max: List[String] = List()
  var inv: Map[String,Set[Predicate]] = Map().withDefaultValue(Set()) // eg, x -> PQR

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
      max ::= big
    }
  }

  // NOT USED in the end...
  @deprecated
  def addPred (v: String, pred:Predicate) {
    inv += v -> (inv(v) + pred)
  }

  def domain(x:String): Set[Predicate] = {
    var res = inv(x)
    if (less contains x)
      for (smaller <- less(x))
        res ++= domain(smaller)
    res
  }

  def + (other: DomainAbst): DomainAbst = {
    val res = new DomainAbst()
    res.less = less
    res.greater = greater
    res.inv = inv

    for ((k,v) <- other.less)
      if (res.less contains k) res.less += k -> (res.less(k) ++ v)
      else res.less += k -> v
    for ((k,v) <- other.greater)
      if (res.greater contains k) res.greater += k -> (res.greater(k) ++ v)
      else res.greater += k -> v
    for ((k,v) <- other.inv)
      if (res.inv contains k) res.inv += k -> (res.inv(k) ++ v)
      else res.inv += k -> v

    res.max = (max ::: other.max) filterNot (res.greater contains _)
    res
  }



  def pp: String = {
    var res = ""
    for ((x,ys) <- greater; y <- ys)
      res += (y + " > " + x+", ")
    if (!greater.isEmpty) res += "\n"
    for ((x,ys) <- less; y <- ys)
      res += (y + " < " + x+", ")
    if (!less.isEmpty) res += "\n"
    for ((v,ps) <- inv)
      res += (v + ": " + ps.mkString(",")+"\n")
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
    res.max = List(bigsmall._1)
    res
  }
  def apply(v: String, pred:Predicate): DomainAbst = {
    val res = new DomainAbst()
    res.inv = Map(v -> Set(pred))
    res
  }
}


object Main extends App {
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