package common.beh

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 16/07/12
 * Time: 12:06
 * To change this template use File | Settings | File Templates.
 */

abstract class Function extends Computation {
  override val isLazy = true

  def calculate(x: Any): Any
}

object Function {
  def apply(body: Any => Any): Function =
    new Function {
      def calculate(x: Any) = try body(x)
        catch {
          case e: scala.MatchError => {}
          case e => throw e
        }
    }

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