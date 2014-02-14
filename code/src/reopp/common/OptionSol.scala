package reopp.common

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 01/08/13.
 */
abstract sealed class OptionSol[+S <: Solution] {
  def get: S
  def isDefined: Boolean
  def getBuffer: Option[Buffer]
}

case class SomeSol[+S <: Solution](s:S) extends OptionSol[S] {
  def get = s
  val isDefined = true
  def getBuffer = s.getBuffer
}

case class NoneSol(b: Option[Buffer]) extends OptionSol[Nothing] {
  def get = throw new NoSuchElementException("NoneSol.get")
  def isDefined = false
  def getBuffer = b
  override def toString = "NoneSol"
}

object NoneSol {
  def apply(): NoneSol = NoneSol(None)
  def apply(b:Buffer): NoneSol = NoneSol(Some(b))
}