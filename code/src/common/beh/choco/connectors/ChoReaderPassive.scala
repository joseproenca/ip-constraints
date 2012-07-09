package common.beh.choco.connectors


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/05/12
 * Time: 19:19
 * To change this template use File | Settings | File Templates.
 */

class ChoReaderPassive(x:String,uid:Int,sizee:Int) extends ChoReader(x,uid,sizee) {

  useData = false
  useCC3 = false

  override def isProactive: Boolean = false

}
