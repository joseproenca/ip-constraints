package common

//import java.util.List
import scala.collection.JavaConversions._
import scala.collection.immutable.Map


/**
 * Caching mechanism for [[common.Function]]'s and [[common.Predicate]]'s:
 * each function and predicate is evaluated with the same argument only once,
 * and the results are buffered.
 *
 * User: jose
 * Date: 13/07/12
 * Time: 11:44
 */

class Buffer {
//  println("buf: "+hashCode)
  private var calculatedP: Map[(Predicate,Any),Boolean] = Map()
  private var calculatedF: Map[(Function,Any),Any] = Map()


  /**
   * Calculate the result of a sequence function using a *caching* mechanism
   * @param functs List of functions f1, f2, .. fn.
   * @param d Data value to be passed to the first function
   * @return fn(..(f2(f1(d))))
   */
  def calculate(functs:List[Function],d:Any): Any = functs match {
    case Nil => d
    case (f: Function)::(fs: List[Function]) =>
      calculate(fs,calculate(f,d))
//      if (calculatedF contains (f,d)) {
////        println("# buffered func #")
//        calculate(fs,calculatedF((f,d)))
//      }
//      else {
//        val res = f.calculate(d)
////        println("# adding "+f+"("+d+") -> "+res+" to buffer "+hashCode())
//        calculatedF += (f,d) -> res
////        print("# Calc func - "+res+" ")
//        calculate(fs,res)
//      }
  }

  /**
   * Calculate the result of function using a *caching* mechanism
   * @param funct Functions f to be applied
   * @param d Data value to be passed to the f
   * @return f(d)
   */
  def calculate(funct: Function, d:Any): Any =
    if (calculatedF contains (funct,d)) {
//              println("# buffered func #")
      calculatedF((funct,d))
    }
    else {
      val res = funct.calculate(d)
//              println("# adding "+funct+"("+d+") -> "+res+" to buffer "+hashCode())
      calculatedF += (funct,d) -> res
//              print("# Calc func - "+res+" ")
      res
    }


  /**
   * Apply functions and then a predicate to a data value, using a *caching* mechanism
   * @param p Predicate to be evaluated
   * @param fs List of functions  f1, f2, ..., fn
   * @param d Dat1a value to be passed to the first function
   * @return p(fn(..(f2(f1(d)))))
   */
  def check(p:Predicate, fs:java.util.List[Function], d:Any) = {
//    println("#### checking "+p+"*"+fs.reverse.mkString(".")+"*"+d+"... ")
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
        if (!data.isDefined || (data.get != d))
          undo.calculate(d)
//        else print(" *** succeeded - not rollingback ***")
    }
//    println(" done.")
  }
}