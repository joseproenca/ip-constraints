package partial

import actors.Actor
import reo.End

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 29/02/12
 * Time: 16:38
 * To change this template use File | Settings | File Templates.
 */

class Worker extends Actor {

  def idlePhase() {
    receive {
      case end:End => {
        /*
        traversal = Traversal(end,strategy)
        newEnds = traversal.extend
        manager ! extend(newEnds) */
        claimingPhase(false)        
      }
      case _ => {}
    }
  }
  
  def claimingPhase(hasSol:Boolean) {
    receive {
      case  _ => {}
    }
  }

  def act() {
    idlePhase()
  }

}


