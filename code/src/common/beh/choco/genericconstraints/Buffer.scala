package common.beh.choco.genericconstraints

//import java.util.List
import scala.collection.JavaConversions._
import common.beh.{UnPredicate, UnFunction}


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 11:44
 * To change this template use File | Settings | File Templates.
 */

class Buffer {
  var calculatedP: Map[(UnPredicate,Any),Boolean] = Map()
  var calculatedF: Map[(UnFunction,Any),Any] = Map()


  def calculate(functs:List[UnFunction],d:Any): Any = functs match {
    case Nil => d
    case (f: UnFunction)::(fs: List[UnFunction]) =>
      if (calculatedF contains (f,d)) {
        println("# buffered func #")
        calculate(fs,calculatedF((f,d)))
      }
      else {
        val res = f.calculate(d)
//        println("# adding "+f+"("+d+") -> "+res+" to buffer")
        calculatedF += (f,d) -> res
        print("# Calc func - "+res+" ")
        calculate(fs,res)
      }
  }

  def check(p:UnPredicate, fs:java.util.List[UnFunction], d:Any) = {
    print("#### checking "+p+"-"+fs.reverse.mkString(".")+"-"+d+"... ")
    val newd = calculate(asJavaIterable(fs).toList.reverse,d)
    calculatedP.get((p, newd)) match {
      case Some(x) =>
        println("# buffered P ####")
        if (x) 1 else 0
      case None =>
        val res = p.check(newd)
//        println("# adding "+p+"("+newd+") -> "+res+" to buffer")
        calculatedP += (p,newd) -> res
        println("# Calc P - "+res+" ####")
        if (res) 1 else 0
    }
  }
}
