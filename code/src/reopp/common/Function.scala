package reopp.common

import scala.collection.JavaConverters._

/**
 * Unary function that is embedded in the [[reopp.common.Constraints]].
 * Transformer channels use these functions in their constraints.
 *
 * Created by jose on 16/07/12.
 */
abstract class Function {
  /** Applies the (untyped) function to a value `x`. */
  def calculate(x: Any): Any
}

object Function {
  /** Constructs a [[reopp.common.Function]] from a scala partial [[scala.Function1]].
    * Typically the function is a block of `case x: Type => ...`. If no case matches it outputs ``():Unit.
    * @param body is the scala [[scala.Function1]].
    * @return new [[reopp.common.Function]] that can be embedded in the synchronous constraints.
    */
  def apply[A]()(body: A => Any): Function =
    new Function {
      def calculate(x: Any) = {
        // calculate is called within a Java program, that cannot generate scala lists properly... (!)
//        x match {
//          case jl: java.util.List[Any] => input = jl.asScala.toList
//          case _ => {}
//        }
        try x match { case y: A => body(y) }
        catch {
          case e: scala.MatchError => {}    // return Unit if undefined
          case e: java.lang.ClassCastException => {}    // return Unit if undefined
          case e => throw e
          // note: 'A' is lost at runtime, so the matchError does not work.
        }
      }
    }

  /**
   * Same as [[reopp.common.Function.apply()]] with a redefined name as `toString`.
   * @param name is the new `toString` value.
   * @param body is the scala [[scala.Function1]]
   * @return the new [[reopp.common.Function]].
   */
  def apply[A](name:String)(body: Any => Any): Function =
    new Function {
      def calculate(x: Any) = {
//        val input = x
//        x match {
//          case jl: java.util.List[Any] => input = jl.asScala.toList
//          case _ => {}
//        }
        try x match { case y: A => body(y) }
        catch {
          case e: scala.MatchError => {}  // return Unit if undefined
          case e: java.lang.ClassCastException => {}    // return Unit if undefined
          case e => throw e
          // note: 'A' is lost at runtime, so the matchError does not work.
        }
      }

      override def toString = name
    }
}