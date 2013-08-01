package reopp.workers

import org.scalatest.FunSpec
import reopp.common.choco.{ChoConstraints, ChoSolution}
import strategies._
import reopp.common.guardedcommands.{GCSolution, Formula}
import reopp.common.guardedcommands.GCConnector.GCBuilder


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 13:15
 * To change this template use File | Settings | File Templates.
 */

class TestWorkers extends FunSpec {
  describe ("Workers full traversal - Writer to a lossy") {
    //    val x = scala.actors.Actor.self
    type S = GCSolution
    type C = Formula
    type St = HybridStrategy[S,C]
    //type SB = StrategyBuilder[S,C,St]

    // create and run deployer
    val deployer = new Deployer[S,C,St](2)
    deployer.start()

    // create nodes
    val wr = new connectors.Writer(2,deployer)
    val lossy = new connectors.Lossy(deployer)

    // link nodes

//    wr.connect()

//    wr.behaviour.connections +=
//      lossy -> Set((wr.behaviour.ends.head,lossy.behaviour.ends.head,lossy.behaviour.uid))
//    wr.neighbours ::= lossy

//    lossy.behaviour.connections +=
//      wr -> Set((lossy.behaviour.ends.head,wr.behaviour.ends.head,wr.behaviour.uid))
//    lossy.neighbours ::= wr

//    wr.connect(lossy , wr.behaviour.ends.head , lossy.behaviour.ends.head)
    lossy(lossy.behaviour.ends.head) <-- wr(wr.behaviour.ends.head)

    // trigger all primitives (only proactive ones will start)
    wr.init
    lossy.init

    Thread.sleep(5000)
    it ("writer is free")
    { assert(wr.owner == None) }
    it ("Lossy is free")
    { assert(lossy.owner == None) }
  }
}
