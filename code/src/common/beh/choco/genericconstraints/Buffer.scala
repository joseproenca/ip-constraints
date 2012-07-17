package common.beh.choco.genericconstraints

import java.util.List
import scala.collection.JavaConversions._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 11:44
 * To change this template use File | Settings | File Templates.
 */

class Buffer {
  var calculated: Map[(UnPredicate,List[UnFunction],Any),Boolean] = Map()

  def check(p:UnPredicate, fs:List[UnFunction], d:Any) = {
    println("####### checking "+p+"-"+fs.reverse.mkString(".")+"-"+d+"... #######")
    calculated.get((p,fs, d)) match {
      case Some(x) => if (x) 1 else 0
      case None =>
        var dt = d
        for (f <- fs) dt = f.calculate(dt)
        val res = p.check(dt)
        calculated += (p,fs,d) -> res
        if (res) 1 else 0
    }
  }
}
