package reopp.workers

//import org.scalatest.FunSpec
import reopp.common.choco.{ChoConstraints, ChoSolution}
import strategies._
import reopp.common.guardedcommands.{GCSolution, Formula}
import reopp.common.guardedcommands.GCConnector.GCBuilder
import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import org.junit.Test
import org.junit.Assert._


/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 13:15
 * To change this template use File | Settings | File Templates.
 */

class TestWorkers {//extends FunSpec {
	    type S = GCSolution
	    type C = Formula
	    type St = HybridStrategy[S,C]
	    //type SB = StrategyBuilder[S,C,St]

  
  //  describe ("Workers full traversal - Writer to a lossy") {
	@Test def TestWorkersWriter2Lossy {
	    //    val x = scala.actors.Actor.self
	
	    // create and run deployer
	    val engine = new Engine[S,C,St](2)
//	    engine.start()
	
	    // create nodes
	    val wr = new connectors.Writer(2)
	    val lossy = new connectors.Lossy
	
	    assertEquals("data not sent",wr.canStart,true) // still has data

	    // link nodes
	
	//    wr.connect()
	
	//    wr.behaviour.connections +=
	//      lossy -> Set((wr.behaviour.ends.head,lossy.behaviour.ends.head,lossy.behaviour.uid))
	//    wr.neighbours ::= lossy
	
	//    lossy.behaviour.connections +=
	//      wr -> Set((lossy.behaviour.ends.head,wr.behaviour.ends.head,wr.behaviour.uid))
	//    lossy.neighbours ::= wr
	
	//    wr.connect(lossy , wr.behaviour.ends.head , lossy.behaviour.ends.head)
	    lossy(lossy.connector.ends.head) <-- wr(wr.connector.ends.head)
	
	    // trigger all primitives (only proactive ones will start)
	    engine.deployer ! Task(wr)
	    engine.deployer ! Task(lossy)
		    
	    engine.awaitTermination
	    // if it terminates, then there are no more workers.

	    assertEquals("data sent",wr.canStart,false) // no more data
	    
	}
	
	@Test def TestVoid {
	  println("Done ;)")
	  assertEquals("all is good",0,0)
	}
	
	@Test def TestSimpleMerger {
	  val engine = GenEngine.oneStep(2)
	  
	  val w1 = engine add
	    writer("w1",List(1,2))	  
	  val w2 = engine add
	    writer("w2",List(3,4))
	  val mrg = engine add
	    merger("in1","in2","out")
	  val rd = engine add
	    reader("rd",3)
	  
	  mrg("in1") <-- w1("w1")
	  mrg("in2") <-- w2("w2")
	  rd("rd") <-- mrg("out")
	  
	  engine.init
	  
	  assertEquals("data not fully sent",w1.canStart || w2.canStart,true) // still more data
	  
	  engine.awaitTermination

//	  	  val w1 = deployer addId
//	    {writer("w1",List(1,2),_)}	  
//	  val w2 = deployer addId
//	    (writer("w2",List(3,4),_))
//	  val mrg = deployer addId
//	    (merger("w1","in2","out",_))
//	  val rd = deployer addId
//	    (reader("rd",3,_))
//	  
//	  mrg("w1") <-- w1("w1")
//	  mrg("in2") <-- w2("w2")
//	  rd("rd") <-- mrg("out")
	  
	}
	
}
