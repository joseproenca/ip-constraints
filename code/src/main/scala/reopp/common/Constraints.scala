package reopp.common

/**
 * Constraints whose solutions ([[reopp.common.Solution]]) describe single synchronous steps.
 * For example, guarded commands and choco expressions are two instances of constraints.
 *
 * Created by jose on 02/05/12.
 */

trait Constraints[S<:Solution[S],C<:Constraints[S,C]] {
//  type mytype <: Constraints[S]

  /** Try to solve the current constraints.
    *
    * @return `Some(s)` if a solution `s` is found, and `None` otherwise.
    */
  def solve(): OptionSol[S] = solve(None:Option[NoneSol])

  /** Try to solve the current constraints.
    *
    * @return `Some(s)` if a solution `s` is found, and `None` otherwise.
    */
  def solve(tried: Option[NoneSol]): OptionSol[S]

  /** Calculate a new constraint after combining `this` with `other`.
    *
    * @param other constraint to be added.
    * @return new combined constraint.
    */
  def ++(other:C): C
  
  /** Creates a new constraint with a unique ID in all of its variables.
   *  @param uid unique ID to be added to the variable names.
   */
  def withID(uid:Int): C
  
//  def update(s:S)
//  def +(other:mytype): mytype
//  def +=(other:Constraints[S])
}
