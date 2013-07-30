package common

import guardedcommands.{GuardedCom, Var, Statement, Formula, True}


/**
 * Handles naming conventions for variables for synchrony, data, and context dependency.
 * Also has other auxiliary functions, and usefull implicit conversions for guarded commands.
 *
 * Created by jose on 06/06/12.
 */
object Utils {
  def flowVar(x: String, uid: Int = 0): String = "F€" + x + "€" + uid
  def dataVar(x: String, uid: Int = 0): String = "D€" + x + "€" + uid
  def predVar(v: String, pred: Any, fs: List[Any]) = v + "#" + pred + "_" + fs.mkString(".")//.hashCode()
  def srcVar(x: String, uid: Int = 0): String = "R€" + x + "€" + uid
  def snkVar(x: String, uid: Int = 0): String = "K€" + x + "€" + uid

  def flow2data(x: String): String = "D" + x.tail
  def data2flow(x: String): String = "F" + x.tail
  def flow2src(x: String):  String = "R" + x.tail
  def flow2snk(x: String):  String = "K" + x.tail
  def var2port(x: String):  String = {
    val y = x.drop(2) . split("€") // y1 is the port
    val z = y(1).split("#")        // z1 is the uid
    y(0)+"_"+z(0)
  }

  def isFlowVar(x: String): Boolean = x.startsWith("F€")
  def isDataVar(x: String): Boolean = x.startsWith("D€")
  def isPredVar(x: String): Boolean = x.contains('#')

  def ppFlowVar(x:String): String = { val y = x.split("€"); placeIndex(y) }
  def ppDataVar(x:String): String = { val y = x.split("€"); "^"+placeIndex(y) }
  def ppPredVar(x:String): String = { val y = x.split("€"); "P"+placeIndex(y) }
  def ppVar(x:String) = if (x.startsWith("F€")) ppFlowVar(x) else ppDataVar(x)
  private def placeIndex(y:Array[String]) =
    if (y(2) == "0") y(1) else y(1)+"_"+y(2)

  // Special treatment of guarded commands
  def mkVar(x: String, uid: Int=0): Var = Var(flowVar(x,uid))
  implicit def var2String(v: Var) = v.name

  implicit def st2GC(s: Statement): GuardedCom = GuardedCom(True,s)
  implicit def gc2GCs(gc: GuardedCom): Formula = Formula(gc)
  implicit def st2GCs(s: Statement): Formula = Formula(GuardedCom(True,s))
  //  implicit def strs2Var(s: String,uid: Int): Var = Var(flowVar(s,uid))
//  implicit def boxint(i:Int): java.lang.Integer = java.lang.Integer.valueOf(i)

}
