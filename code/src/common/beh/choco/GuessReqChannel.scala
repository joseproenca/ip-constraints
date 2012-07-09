package common.beh.choco

import common.beh.{Constraints, Solution, Behaviour}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 27/06/12
 * Time: 14:12
 * To change this template use File | Settings | File Templates.
 */

trait GuessReqChannel[S<: Solution, C <: Constraints[S,C]] extends Behaviour[S,C] {

  val List(x,y) = ends

  def guessRequirements(end: String) = if (end == x) Set(y) else Set(x)
}
