package common.beh


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:20
 * To change this template use File | Settings | File Templates.
 */

object Utils {
  def flowVar(x: String, uid: Int): String = "F$" + x + "$" + uid

  def dataVar(x: String, uid: Int): String = "D$" + x + "$" + uid

  def predVar(v: String, pred: Any, fs: List[Any]) = v + "#" + pred + "_" + fs.mkString(".")//.hashCode()

  def flow2data(x: String): String = "D" + x.tail

  def data2flow(x: String): String = "F" + x.tail

  def isFlowVar(x: String): Boolean = x.startsWith("F$")

  def isDataVar(x: String): Boolean = x.startsWith("D$")

  def isPredVar(x: String): Boolean = x.contains('#')

  def ppFlowVar(x:String): String = { val y = x.split("\\$"); y(1)+"_"+y(2) }
  def ppDataVar(x:String): String = { val y = x.split("\\$"); "^"+y(1)+"_"+y(2) }
  def ppPredVar(x:String): String = { val y = x.split("\\$"); "P"+y(1)+"_"+y(2) }
  def ppVar(x:String) = if (x.startsWith("F$")) ppFlowVar(x) else ppDataVar(x)
}
