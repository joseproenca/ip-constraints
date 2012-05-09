package common.beh

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 09:05
 * To change this template use File | Settings | File Templates.
 */

trait Constraints[+S<:Solution,C<:Constraints[S,C]] {
//  type mytype <: Constraints[S]

  def solve: Option[S]

  def ++(other:C): C
//  def update(s:S)
//  def +(other:mytype): mytype
//  def +=(other:Constraints[S])
}
