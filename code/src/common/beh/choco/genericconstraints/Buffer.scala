package common.beh.choco.genericconstraints

//import java.util.List
import scala.collection.JavaConversions._


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
        print("# bf ")
        calculate(fs,calculatedF((f,d)))
      }
      else {
        println("# Calc ")
        val res = f.calculate(d)
        calculatedF += (f,d) -> res
        calculate(fs,res)
      }
  }

  def check(p:UnPredicate, fs:java.util.List[UnFunction], d:Any) = {
    println("####### checking "+p+"-"+fs.reverse.mkString(".")+"-"+d+"... #######")
    val newd = calculate(asJavaIterable(fs).toList.reverse,d)
    calculatedP.get((p, newd)) match {
      case Some(x) =>
        println("# buffered #")
        if (x) 1 else 0
      case None =>
        println("# Calc #")
        var dt = d
        for (f <- fs) dt = f.calculate(dt)
        val res = p.check(dt)
        calculatedP += (p,d) -> res
        if (res) 1 else 0
    }
  }
}
