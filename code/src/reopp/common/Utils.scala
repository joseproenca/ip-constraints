package reopp.common

import guardedcommands.{GuardedCom, Var, Statement, Formula, True}


/**
 * Handles naming conventions for variables for synchrony, data, and context dependency.
 * Also has other auxiliary functions, and usefull implicit conversions for guarded commands.
 *
 * Created by jose on 06/06/12.
 */
object Utils {
  def mkFlowVar(x: String): String = "F€" + x
  def mkDataVar(x: String): String = "D€" + x
  def mkPredVar(v: String, pred: Any, fs: List[Any]) = v + "#" + pred + "_" + fs.mkString(".")//.hashCode()
  def mkSrcVar(x: String): String = "R€" + x
  def mkSnkVar(x: String): String = "K€" + x

  def toDataVar(x: String): String = "D" + x.tail
  def toFlowVar(x: String): String = "F" + x.tail
  def toSrcVar(x: String):  String = "R" + x.tail
  def toSnkVar(x: String):  String = "K" + x.tail
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
  def ppPredVar(x:String): String = { val y = x.split("€"); "P_"+placeIndex(y) }
  def ppVar(x:String) = if (x.startsWith("F€")) ppFlowVar(x) else ppDataVar(x)
  private def placeIndex(y:Array[String]) =
    if (y.size > 2) {
    	if (y(2) == "0") y(1) else y(1)+"["+y(2).substring(6)+"]"
    }
    else if (y.size > 1) y(1) else ""
//      if (y.isEmpty) "" else y(1)

  // append IDs to variable names
  def addID(n:String,id:Int): String = n+"€"+id
      
  // Special treatment of guarded commands
  def mkVar(x: String): Var = Var(mkFlowVar(x))
  implicit def var2String(v: Var) = v.name

  implicit def st2GC(s: Statement): GuardedCom = GuardedCom(True,s)
  implicit def gc2GCs(gc: GuardedCom): Formula = Formula(gc)
  implicit def st2GCs(s: Statement): Formula = Formula(GuardedCom(True,s))
  //  implicit def strs2Var(s: String,uid: Int): Var = Var(flowVar(s,uid))
//  implicit def boxint(i:Int): java.lang.Integer = java.lang.Integer.valueOf(i)

}
