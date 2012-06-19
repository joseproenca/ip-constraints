package workers

import connectors.Merger
import org.scalatest.FunSpec
import common.beh.choco.{ChoConstraints, ChoSolution}
import strategies._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 09/05/12
 * Time: 17:07
 * To change this template use File | Settings | File Templates.
 */

class TestMergersWorkers extends FunSpec {
  type S = ChoSolution
  type C = ChoConstraints
  type St = CompleteStrategy[S,C]
  type SB = StrategyBuilder[S,C,St]
  type D = Deployer[S,C,St,SB]
  type Nd = Node[S,C]

  type St2 = OneStepStrategy[S,C]
  type SB2 = StrategyBuilder[S,C,St2]
  type D2 = Deployer[S,C,St2,SB2]

  var size = 2
  var workers = 2

//  describe ("Workers full traversal - tree of merges.") {
//    // create and run deployer
//    val deployer = new Deployer[S,C,St,SB](workers,CompleteStrategyBuilder)
//    deployer.start()
//
//    // create reader
//    val rd = new connectors.Reader(math.pow(workers,size).toInt,deployer)
//    println("reader: "+rd.behaviour.uid)
//
//    // create and connect mergers
//    val ends = createBranches[St,SB,D](size,Set((rd,rd.behaviour.ends.head)),deployer)
//
//    // create and connect writers
//    for ((node,end) <- ends) {
//      val wr = new connectors.Writer(1,deployer)
//      println("writer: "+wr.behaviour.uid)
//
//      node.behaviour.connections +=
//        wr -> Set((end,wr.behaviour.ends.head,wr.behaviour.uid))
//      wr.behaviour.connections +=
//        node -> Set((wr.behaviour.ends.head,end,node.behaviour.uid))
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

  describe ("Workers one step traversal - tree of merges.") {
    // create and run deployer
    val deployer = new Deployer[S,C,St2,SB2](workers,OneStepStrategyBuilder)
    deployer.start()

    // create reader
    val rd = new connectors.Reader(math.pow(workers,size).toInt,deployer)
    println("reader: "+rd.behaviour.uid)
//    println(" - "+rd.hashCode())

    // create and connect mergers
    val ends = createBranches[St2,SB2,D2](size,Set((rd,rd.behaviour.ends.head)),deployer)

    // create and connect writers
    var writers = List[Nd]()
    for ((node,end) <- ends) {
      val wr = new connectors.Writer(1,deployer)
      writers ::= wr
      println("writer: "+wr.behaviour.uid)
//      println(" - "+wr.hashCode())

      node.connect(wr,end,wr.behaviour.ends.head)

    }

    // start readers and writers
    rd.init()
    for (wr <- writers)
      wr.init()

    Thread.sleep(9000)
    it ("reader is free")
    { assert(rd.owner == None) }
  }




  private def createBranch[Stt<:Strategy[S,C,Stt],SBB<:StrategyBuilder[S,C,Stt],DD<:Deployer[S,C,Stt,SBB]]
    (from:Nd, end:String, deployer:DD): (Nd,String,String) = {
    val merger: Nd = new Merger(deployer)
//      new Node[S,C](deployer) {
//      val uid = this.hashCode()
//      val behaviour = new ChoMerger("a","b","c",uid)
//      // what ends depend on "end" - just a guess to decide when to search for a solution
//      def dependsOn(end: String) = Set()
//    }
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
