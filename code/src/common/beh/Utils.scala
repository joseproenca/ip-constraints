package common.beh

import guardedcommands._
import guardedcommands.GuardedCom
import guardedcommands.Var


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:20
 * To change this template use File | Settings | File Templates.
 */

object Utils {
  def mkVar(x: String, uid: Int=0): Var = Var(flowVar(x,uid))
  def flowVar(x: String, uid: Int): String = "F€" + x + "€" + uid
  def dataVar(x: String, uid: Int): String = "D€" + x + "€" + uid
  def predVar(v: String, pred: Any, fs: List[Any]) = v + "#" + pred + "_" + fs.mkString(".")//.hashCode()
  def srcVar(x: String, uid: Int): String = "R€" + x + "€" + uid
  def snkVar(x: String, uid: Int): String = "K€" + x + "€" + uid

  def flow2data(x: String): String = "D" + x.tail
  def data2flow(x: String): String = "F" + x.tail
  def flow2src(x: String):  String = "R" + x.tail
  def flow2snk(x: String):  String = "K" + x.tail

  def isFlowVar(x: String): Boolean = x.startsWith("F€")
  def isDataVar(x: String): Boolean = x.startsWith("D€")
  def isPredVar(x: String): Boolean = x.contains('#')

  implicit def st2GC(s: Statement): GuardedCom = GuardedCom(True,s)
  implicit def gc2GCs(gc: GuardedCom): GuardedCommands = GuardedCommands(gc)
  implicit def st2GCs(s: Statement): GuardedCommands = GuardedCommands(GuardedCom(True,s))
  //  implicit def strs2Var(s: String,uid: Int): Var = Var(flowVar(s,uid))

  def ppFlowVar(x:String): String = { val y = x.split("€"); y(1)+"_"+y(2) }
  def ppDataVar(x:String): String = { val y = x.split("€"); "^"+y(1)+"_"+y(2) }
  def ppPredVar(x:String): String = { val y = x.split("€"); "P"+y(1)+"_"+y(2) }
  def ppVar(x:String) = if (x.startsWith("F€")) ppFlowVar(x) else ppDataVar(x)
}
