package dreams

import org.scalatest.FunSpec
import common.guardedcommands.dataconnectors.GCLossy

import common.guardedcommands.{Formula, GCSolution}
import common.Utils
import Utils._
import common.guardedcommands.GCConnector.GCBuilder

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
    type Sol = GCSolution
    type Constr = Formula
    
    val wr = new connectors.Writer(2)
    {
      var counter = 0
      override protected def processSol(sol:Sol,freshSol:Boolean): Nothing = {
        val a = mkVar("a",uid)

        if (freshSol) println("counter! "+counter)
        if (freshSol && counter <2 ){// && sol.hasFlow(flowVar("a",hashCode())) ) { //&& sol.sizeModel == 2) {
          counter +=1
          it ("has solution "+counter+"-"+hashCode())
          //{assert(sol.sizeModel == 2)}
          {assert(sol hasFlowOn a)}
        }
        else if (freshSol && counter == 2 ) {//&& sol.sizeModel == 0){
          it ("has no more solution")
//          {assert(sol.sizeModel == 2)}
          {assert(sol hasFlowOn a)}
          counter =3
        }
        else if (freshSol)
          it ("has no other options: counter/flow "+counter+"/"+sol.hasFlowOn(a))
           //: counter/size = "+counter+"/"+sol.sizeModel)
          {assert(false)}
        super.processSol(sol,freshSol)
      }
    }

    val lossy = Actor[Sol,Constr](uid => new GCLossy("x","y",uid))
               //new connectors.Lossy


    lossy(lossy.behaviour.ends.head) <-- wr(wr.behaviour.ends.head)


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
