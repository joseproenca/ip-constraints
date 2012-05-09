package common.beh

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 08:52
 * To change this template use File | Settings | File Templates.
 */

abstract class Behaviour[S<: Solution, C <: Constraints[S,C]](val ends: List[String],val uid: Int) {
//  protected type mytype <: Behaviour[S, C]
//  val ends: List[String]
//  val uid: Int
  var constraints: C
  
  var connections: Map[AnyRef,Set[(String,String,Int)]] = Map() // neighbours to pairs of sync'd ends

//  def +(other:mytype): mytype

  //def join(c1:C, c2:C): C

  // adds to "c" the sync constraints wrt the ends shared with "from"
  def sync(from:AnyRef,c:C): C

  // adds to "c" the border constraints wrt the ends shared with "from"
  def border(from:AnyRef,c:C): C

  def update(s: S)

  def isProactive = false // default: false

  def noSol: S

  // def dependsOn(ends:List[String],s:Solution)
  def dataOn(end:String,s:S) : Any
}
