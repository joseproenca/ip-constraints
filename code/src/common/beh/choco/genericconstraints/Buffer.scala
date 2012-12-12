package common.beh.choco.genericconstraints

//import java.util.List
import scala.collection.JavaConversions._
import common.beh.{Predicate, Function}
import scala.collection.immutable.Map
import common.beh.Solution


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 11:44
 * To change this template use File | Settings | File Templates.
 */

class Buffer {
//  println("buf: "+hashCode)
  private var calculatedP: Map[(Predicate,Any),Boolean] = Map()
  private var calculatedF: Map[(Function,Any),Any] = Map()


  def calculate(functs:List[Function],d:Any): Any = functs match {
    case Nil => d
    case (f: Function)::(fs: List[Function]) =>
      if (calculatedF contains (f,d)) {
//        println("# buffered func #")
        calculate(fs,calculatedF((f,d)))
      }
      else {
        val res = f.calculate(d)
//        println("# adding "+f+"("+d+") -> "+res+" to buffer "+hashCode())
        calculatedF += (f,d) -> res
//        print("# Calc func - "+res+" ")
        calculate(fs,res)
      }
  }

  def check(p:Predicate, fs:java.util.List[Function], d:Any) = {
//    println("#### checking "+p+"-"+fs.reverse.mkString(".")+"-"+d+"... ")
    val newd = calculate(asJavaIterable(fs).toList.reverse,d)
    calculatedP.get((p, newd)) match {
      case Some(x) =>
//        println("(buffered)")
        if (x) 1 else 0
      case None =>
        val res = p.check(newd)
//        println("# adding "+p+"("+newd+") -> "+res+" to buffer")
        calculatedP += (p,newd) -> res
//        println("# Calc P - "+res+" ####")
        if (res) 1 else 0
    }
  }

  /**
   * Apply 'undo' to data 'd' for every calculation of 'f'(d) except if 'd'='data'.
   * Not optimised - Iterating over all buffered applications of functions.
   * (We could modify the 'calculatedF' map to be a nested map to avoid iteration.)
   * @param f function to be reverted
   * @param undo reverting function
   * @param data possible successful data, that must not be reverted
   */
  def rollback(f: Function, undo: Function, data: Option[Any]) {
//    print("rollbacking "+f+"("+data+") with "+undo)
//    print(" @"+hashCode+calculatedF.keys.mkString("[",",","]"))
    for ((f2,d) <- calculatedF.keys) {
//      print(" - "+f2+"("+d+")")
      if (f2 == f)
        if (!data.isDefined || (data.get != d)) undo.calculate(d)
    }
//    println(" done.")
  }
}
