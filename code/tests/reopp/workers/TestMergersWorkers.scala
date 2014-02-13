package reopp.workers

import connectors.Merger
//import org.scalatest.FunSpec
import reopp.common.choco.{ChoConstraints, ChoSolution}
import strategies._
import reopp.common.guardedcommands.{GCSolution, Formula}
import reopp.common.guardedcommands.GCConnector.GCBuilder
import org.junit.Test
import org.junit.Assert._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 17:07
 * To change this template use File | Settings | File Templates.
 */

class TestMergersWorkers { //extends FunSpec {
  type S = GCSolution
  type C = Formula
  type St = CompleteStrategy[S,C]
//  type SB = StrategyBuilder[S,C,St]
  type D = Deployer[S,C,St]
  type Nd = Node[S,C]

  type St2 = OneStepStrategy[S,C]
//  type SB2 = StrategyBuilder[S,C,St2]
  type D2 = Deployer[S,C,St2]

  var size = 2
  var workers = 2

//  describe ("Workers full traversal - tree of merges.") {
//    // create and run deployer
//    val deployer = new Deployer[S,C,St,SB](reopp.workers,CompleteStrategyBuilder)
//    deployer.start()
//
//    // create reader
//    val rd = new connectors.Reader(math.pow(reopp.workers,size).toInt,deployer)
//    println("reader: "+rd.connector.uid)
//
//    // create and connect mergers
//    val ends = createBranches[St,SB,D](size,Set((rd,rd.connector.ends.head)),deployer)
//
//    // create and connect writers
//    for ((node,end) <- ends) {
//      val wr = new connectors.Writer(1,deployer)
//      println("writer: "+wr.connector.uid)
//
//      node.connector.connections +=
//        wr -> Set((end,wr.connector.ends.head,wr.connector.uid))
//      wr.connector.connections +=
//        node -> Set((wr.connector.ends.head,end,node.connector.uid))
//      node.neighbours ::= wr
//      wr.neighbours ::= node
//    }
//
//    // enough to trigger the reader.
//    rd.init
//
//    Thread.sleep(5000)
//    it ("reader is free")
//    { assert(rd.owner == None) }
//    println("-----------------")
//  }

  size = 4
  workers = 2

  //describe ("Workers one step traversal - tree of merges.") {
  @Test def TestOneStepTraversalTree() {
    // create and run deployer
    val deployer = new Deployer[S,C,St2](workers)
    deployer.start()

    // create reader
    val rd = new connectors.Reader(math.pow(workers,size).toInt)
    println("reader: "+rd.connector.uid)
//    println(" - "+rd.hashCode())

    // create and connect mergers
    val ends = createBranches[St2](size,Set((rd,rd.connector.ends.head)))

    // create and connect writers
    var writers = List[Nd]()
    for ((node,end) <- ends) {
      val wr = new connectors.Writer(1)
      writers ::= wr
      println("writer: "+wr.connector.uid)
//      println(" - "+wr.hashCode())

//      node.connect(wr,end,wr.connector.ends.head)
      node(end) <-- wr(wr.connector.ends.head)

    }

    // start readers and writers
    deployer ! Task(rd) //rd.init()
    for (wr <- writers)
      deployer ! Task(wr) //wr.init()

//    Thread.sleep(9000)
//    it ("reader is free")
//    { assert(rd.owner == None) }
//    assertEquals("No workers",deployer.currentWorkers,0)
//    assertEquals("No tasks",deployer.pendingTasks.size,0)
      
      // it will halt forever unless there are no more workers and tasks.
      deployer.latch.await() 
  }




  private def createBranch[Stt<:Strategy[S,C,Stt]]
    (from:Nd, end:String): (Nd,String,String) = {
    val merger: Nd = new Merger

    from(end) <-- merger("c")

    (merger,"a","b")
  }

  private def createBranches[Stt<:Strategy[S,C,Stt]]
    (n:Int,sinks:Set[(Nd,String)]): Set[(Nd,String)] = {
    if (n<=0) sinks
    else {
      var sources = Set[(Nd,String)]()
      for (sk <- sinks) {
        val (n,e1,e2) = createBranch[Stt](sk._1,sk._2)
        sources = sources ++ Set((n,e1),(n,e2))
      }
      createBranches[Stt](n-1,sources)
    }
  }
}
