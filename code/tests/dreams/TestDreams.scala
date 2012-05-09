package dreams

import org.scalatest.FunSpec
import common.beh.choco.ChoSolution

/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 07/05/12
 * Time: 15:00
 * To change this template use File | Settings | File Templates.
 */

class TestDreams extends FunSpec {


  describe ("Dreams - Writer to a lossy") {
//    val x = scala.actors.Actor.self
    
    val wr = new connectors.Writer(2)
    {
      var counter = 0
      override protected def processSol(sol:ChoSolution,freshSol:Boolean): Nothing = {
        if (freshSol) println("counter! "+counter)
        if (freshSol && counter <2 && sol.sizeModel == 2) {
          counter +=1
          it ("has solution "+counter+"-"+hashCode())
          {assert(sol.sizeModel == 2)}
        }
        else if (freshSol && counter == 2 && sol.sizeModel == 0){
          it ("has no more solution")
          {assert(sol.sizeModel == 2)}
          counter =3
        }
        else if (freshSol)
          it ("has no other options")
          {assert(false)}
        super.processSol(sol,freshSol)
      }
    }
    val lossy = new connectors.Lossy

    wr.behaviour.connections +=
      lossy -> Set((wr.behaviour.ends.head,lossy.behaviour.ends.head,lossy.behaviour.uid))

    lossy.behaviour.connections +=
      wr -> Set((lossy.behaviour.ends.head,wr.behaviour.ends.head,wr.behaviour.uid))

    wr.start()
    lossy.start()

    wr ! Connect(lossy)
    lossy ! Connect(wr)

    wr ! Release
    lossy ! Release

    Thread.sleep(2000)
    it ("writer idle")
    { assert(wr.isIdle) }
  }
}
