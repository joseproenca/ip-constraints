package common


/**
 * Unary function that is embedded in the [[common.Constraints]].
 * Transformer channels use these functions in their constraints.
 *
 * Created by jose on 16/07/12.
 */
abstract class Function {
  /** Applies the (untyped) function to a value `x`. */
  def calculate(x: Any): Any
}

object Function {
  /** Constructs a [[common.Function]] from a scala partial [[scala.Function1]].
    * Typically the function is a block of `case x: Type => ...`. If no case matches it outputs ``():Unit.
    * @param body is the scala [[scala.Function1]].
    * @return new [[common.Function]] that can be embedded in the synchronous constraints.
    */
  def apply()(body: Any => Any): Function =
    new Function {
      def calculate(x: Any) = try body(x)
        catch {
          case e: scala.MatchError => {}
          case e => throw e
        }
    }

  /**
   * Same as [[common.Function.apply()]] with a redefined name as `toString`.
   * @param name is the new `toString` value.
   * @param body is the scala [[scala.Function1]]
   * @return the new [[common.Function]].
   */
  def apply(name:String)(body: Any => Any): Function =
    new Function {
      def calculate(x: Any) = try body(x)
      catch {
        case e: scala.MatchError => {}
        case e => throw e
      }
      override def toString = name
    }
}