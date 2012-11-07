package common.beh

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 08:57
 * To change this template use File | Settings | File Templates.
 */

trait Solution {
  def hasFlow(end: String): Boolean
  def dataOn(end: String): Option[Any]
  def pretty: String
}

trait EmptySol[A <: Solution] {
  def sol: A
}

//object NoSol extends Solution{
//  def hasFlow(end: String) = false
//  def pretty = "No Solution"
//}
