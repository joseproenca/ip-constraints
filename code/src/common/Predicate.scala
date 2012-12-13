package common

/**
 * Unary predicate that is embedded in the [[common.Constraints]].
 * Predicate channels use these predicates in their constraints.
 *
 * Created by jose on 13/07/12.
 */
abstract class Predicate {
  /** Checks if the predicate holds for a given `x`. */
  def check(x: Any): Boolean

  //  def opposite = new UnPred {
  //    def check(x: Any) = this.check(x)
  //  }
}

object Predicate {
  /**
   * Constructs a [[common.Predicate]] from a scala partial [[scala.Function1]].
   * Typically the function is a block of `case x: Type => ...`. If no case matches it outputs `false`.
   * @param body is the scala [[scala.Function1]].
   * @return new [[common.Predicate]] that can be embedded in the synchronous constraints.
   */
  def apply(body: Any => Boolean): Predicate = new Predicate {
    def check(x: Any) = try body(x)
    catch {
      case e: scala.MatchError => false
      case e => throw e
    }
  }

  /**
   * Same as [[common.Predicate.apply()]] with a redefined name as `toString`.
   * @param name is the new `toString` value.
   * @param body is the scala [[scala.Function1]]
   * @return the new [[common.Predicate]].
   */
  def apply(name: String)(body: Any => Boolean): Predicate = new Predicate {
    def check(x: Any) = try body(x)
    catch {
      case e: scala.MatchError => false
      case e => throw e
    }
    override def toString = name
  }
}
