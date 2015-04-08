package reopp.common.choco.dataconnectors

import reopp.common.choco._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 06/06/12
 * Time: 17:12
 * To change this template use File | Settings | File Templates.
 */

// Readers do not change
class ChoReader(x: String, uid: Int, size: Int) extends connectors.ChoReader(x, uid, size) {

  useData = true
  useCC3 = false

}