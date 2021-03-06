package reopp.workers

import connectors.Merger
//import org.scalatest.FunSpec
import org.junit.Test
import org.junit.Assert._
import reopp.common.choco.{ChoConstraints, ChoSolution}
import strategies._
import reopp.common.guardedcommands.{Formula, GCSolution}
import reopp.common.guardedcommands.GCConnector.GCBuilder

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 15/05/12
 * Time: 12:21
 * To change this template use File | Settings | File Templates.
 */

class TestMergersHybrid { //extends FunSpec {
  type S = GCSolution
  type C = Formula
  type St = HybridStrategy[S,C]
//  type SB = StrategyBuilder[S,C,St]
  type D = Deployer[S,C,St]
  type Nd = Node[S,C]

  var size = 2
  var workers = 20
  var endtime = 0

//  describe ("Workers hybrid strategy - tree of merges.") {
  @Test def TestHybridTraversalTree() {
    // create and run deployer
    val engine = new Engine[S,C,St](workers)
//    deployer.start()

    println("height: "+size)

    // create reader
    var missing = math.pow(2,size).toInt
    val rd = new connectors.Reader(missing.toInt)

    val lock:scala.concurrent.Lock = new scala.concurrent.Lock()
    val thread = Thread.currentThread()
//    println("reader: "+rd.connector.uid)
    rd.connector.listen(() => {
      lock.acquire()
      missing -=1
      lock.release()
      println ("["+missing+"]")
      if (missing==0) thread.interrupt()
    })
    //    println(" - "+rd.hashCode())

    // create and connect mergers
    val ends = createBranches[St](size,Set((rd,rd.connector.ends.head)))

    // create and connect writers
    var writers = List[Nd]()
    for ((node,end) <- ends) {
      val wr = new connectors.Writer(1)
      writers ::= wr
//      println("writer: "+wr.connector.uid)
      //      println(" - "+wr.hashCode())

      // ORDER IMPORTANT: source then sink!
      //node.connect(wr,end,wr.connector.ends.head)
      node(end) <-- wr(wr.connector.ends.head)
    }

    // start readers and writers
    val t1 = System.currentTimeMillis()

    for (wr <- writers)
      engine.deployer ! Task(wr) //wr.init()
    engine.deployer ! Task(rd) //rd.init()

    try {
      engine.awaitTermination
//      Thread.sleep(30000)
    }
    catch {
      case e:java.lang.InterruptedException =>
        val t2 = System.currentTimeMillis()
        println("time: "+(t2 - t1))

//        it ("reader is free")
//        { assert(rd.connector.size == 0) }
        assertEquals("reader is free",rd.connector.size, 0)
    }

  }




  private def createBranch[Stt<:Strategy[S,C,Stt]]
  (from:Nd, end:String): (Nd,String,String) = {
    val merger: Nd = new Merger
    //    println(" - "+merger.hashCode())

//    from.connect(merger,end,"c")
//    merger.connect(from,"c",end) // order important (sink then source)

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
