package common.beh

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 13/07/12
 * Time: 10:47
 * To change this template use File | Settings | File Templates.
 */
abstract class Predicate {
  def check(x: Any): Boolean

  //  def opposite = new UnPred {
  //    def check(x: Any) = this.check(x)
  //  }
}

object Predicate {
  def apply(body: Any => Boolean): Predicate = new Predicate {
    def check(x: Any) = try body(x)
    catch {
      case e: scala.MatchError => false
      case e => throw e
    }
  }

  def apply(name: String)(body: Any => Boolean): Predicate = new Predicate {
    def check(x: Any) = try body(x)
    catch {
      case e: scala.MatchError => false
      case e => throw e
    }
    override def toString = name
  }
}
