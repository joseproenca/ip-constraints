package workers

import connectors.Merger
import org.scalatest.FunSpec
import common.beh.choco.{ChoConstraints, ChoSolution}
import strategies._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 15/05/12
 * Time: 12:21
 * To change this template use File | Settings | File Templates.
 */

class TestMergersHybrid extends FunSpec {
  type S = ChoSolution
  type C = ChoConstraints
  type St = HybridStrategy[S,C]
  type SB = StrategyBuilder[S,C,St]
  type D = Deployer[S,C,St,SB]
  type Nd = Node[S,C]

  var size = 3
  var workers = 20
  var endtime = 0

  describe ("Workers hybrid strategy - tree of merges.") {
    // create and run deployer
    val deployer = new Deployer[S,C,St,SB](workers,HybridStrategyBuilder)
    deployer.start()

    println("height: "+size)

    // create reader
    var missing = math.pow(2,size).toInt
    val rd = new connectors.Reader(missing.toInt,deployer)

    val lock:scala.concurrent.Lock = new scala.concurrent.Lock()
    val thread = Thread.currentThread()
//    println("reader: "+rd.behaviour.uid)
    rd.behaviour.listen(() => {
      lock.acquire()
      missing -=1
      lock.release()
      println ("["+missing+"]")
      if (missing==0) thread.interrupt()
    })
    //    println(" - "+rd.hashCode())

    // create and connect mergers
    val ends = createBranches[St,SB,D](size,Set((rd,rd.behaviour.ends.head)),deployer)

    // create and connect writers
    var writers = List[Nd]()
    for ((node,end) <- ends) {
      val wr = new connectors.Writer(1,deployer)
      writers ::= wr
//      println("writer: "+wr.behaviour.uid)
      //      println(" - "+wr.hashCode())

      node.connect(wr,end,wr.behaviour.ends.head)
    }

    // start readers and writers
    val t1 = System.currentTimeMillis()

    for (wr <- writers)
      wr.init()
    rd.init()

    try {
      Thread.sleep(30000)
    }
    catch {
      case e:java.lang.InterruptedException =>
        val t2 = System.currentTimeMillis()
        println("time: "+(t2 - t1))

        it ("reader is free")
        { assert(rd.behaviour.size == 0) }
    }

  }




  private def createBranch[Stt<:Strategy[S,C,Stt],SBB<:StrategyBuilder[S,C,Stt],DD<:Deployer[S,C,Stt,SBB]]
  (from:Nd, end:String, deployer:DD): (Nd,String,String) = {
    val merger: Nd = new Merger(deployer)
    //    println(" - "+merger.hashCode())

    from.connect(merger,end,"c")

    (merger,"a","b")
  }

  private def createBranches[Stt<:Strategy[S,C,Stt],SBB<:StrategyBuilder[S,C,Stt],DD<:Deployer[S,C,Stt,SBB]]
  (n:Int,sinks:Set[(Nd,String)],deployer:DD): Set[(Nd,String)] = {
    if (n<=0) sinks
    else {
      var sources = Set[(Nd,String)]()
      for (sk <- sinks) {
        val (n,e1,e2) = createBranch[Stt,SBB,DD](sk._1,sk._2,deployer)
        sources = sources ++ Set((n,e1),(n,e2))
      }
      createBranches[Stt,SBB,DD](n-1,sources,deployer)
    }
  }
}
