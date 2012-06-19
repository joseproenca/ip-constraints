package common.beh

import choco.dataconnectors.Predicate

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

  def predVar(v: String, pred: Predicate) = v + "_" + pred //.hashCode()

  def flow2data(x: String): String = "D" + x.tail

  def isFlowVar(x: String): Boolean = x.startsWith("F$")
}
