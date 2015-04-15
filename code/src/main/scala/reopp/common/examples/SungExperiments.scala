package reopp.common.examples

import java.util.concurrent.Semaphore

import reopp.common.{Predicate, Utils, OptionSol, Function}
import reopp.common.guardedcommands.{GCSolution, GCConnector}
import reopp.common.guardedcommands.dataconnectors.ConnectorGen._
import Utils._


/**
 * Created by jose on 14/04/15.
 */
object SungExperiments extends App {


  /////  SIZE OF CONNECTORS
  val NUMSYNC = 50
  val NUMTRANS = 50
  val NUMFILTERS = 200

  /**
   * Uses a semaphore (not yet shared) to block execution while waiting for an update.
   */
  class InputPortImpl(port: String, sem: Semaphore) extends GCConnector(List(port)) {

    private var data: Option[Any] = None

    def getConstraints = data match {
      case None    => !port
      case Some(v) => port --> (port :== v)
    }

    override def update(s: OptionSol[GCSolution]) {
      if (s.isDefined && (s.get hasFlowOnPort port)) {
        data = None
        sem.release()
      }
    }

    def put(d:Any): Unit = {
      data = Some(d)
      sem.acquire()
    }
  }

  // counts the time to execute an action
  def time(fun: => OptionSol[GCSolution]): Long = {
    val t = System.currentTimeMillis()
    //val res = cons.solveChocoSat
    fun
    val spent = System.currentTimeMillis() - t
    println(s"took $spent secs.")
    spent
  }



  ////// sequence of syncs ///////

  val myInputPort = new InputPortImpl("1", new Semaphore(1))

  def syncs(k: Int): GCConnector = {
    var res: GCConnector = myInputPort
    for (i <- 1 to k)
      res ++= sync(i.toString,(i+1).toString)
    res
  }

  myInputPort.put(())
  val conn = syncs(NUMSYNC)
  val const = conn.getConstraints

  println("---- syncs ----")
  time(const.solveXZ3)
  time(const.quickDataSolveSAT4J)
  time(const.quickDataSolveZ3)
  time(const.solveChocoDyn)
  time(const.solveChocoPredAbstVarOrdered)



//  val sol1 = const.solveXZ3
//  val sol2 = const.quickDataSolveSAT4J
//  val sol3 = const.quickDataSolveZ3
//  val sol4 = const.solveChocoDyn
//  val sol5 = const.solveChocoPredAbstVarOrdered
//
//  println(s"sol1:\n$sol1")
//  println(s"sol2:\n$sol2")
//  println(s"sol3:\n$sol3")
//  println(s"sol4:\n$sol4")
//  println(s"sol5:\n$sol5")




  ////// sequence of transformers ///////

  val myInputPort2 = new InputPortImpl("1", new Semaphore(1))
  myInputPort2.put(0)

  val succ = Function("succ") {
    case i:Int => i+1
    case x =>
      println(s"UNKNOWN x received - $x : ${x.getClass}")
      42
  }

  def transfs(k: Int): GCConnector = {
    var res: GCConnector = myInputPort2
    for (i <- 1 to k)
      res ++= transf(i.toString,(i+1).toString,succ)
    res
  }

//  myInputPort.put(())
  val conn2 = transfs(NUMTRANS)
  val const2 = conn2.getConstraints

  println("---- transformers ----")
//  time(const2.solveXZ3)            // NOT CALLING EXTERNAL! wrong.
  time(const2.quickDataSolveSAT4J)
  time(const2.quickDataSolveZ3)
  time(const2.solveChocoDyn)
  time(const2.solveChocoPredAbstVarOrdered)



//  val sol21 = const2.solveXZ3 // buggy...
//  val sol22 = const2.quickDataSolveSAT4J
//  val sol23 = const2.quickDataSolveZ3
//  val sol24 = const2.solveChocoDyn
//  val sol25 = const2.solveChocoPredAbstVarOrdered

//  println(s"sol21:\n$sol21")
//  println(s"sol22:\n$sol22")
//  println(s"sol23:\n$sol23")
//  println(s"sol24:\n$sol24")
//  println(s"sol25:\n$sol25")



  ////// sequence of filters - only the last fails ///////

  val myInputPort3 = new InputPortImpl("1", new Semaphore(1))
  myInputPort3.put(0)

  def is(v:Int) = Predicate("is-"+v) { // ATTENTION: name must be unique for each v! (except for chocoDyn)
    case i:Int => i != v
    case x =>
      println(s"UNKNOWN x received - $x : ${x.getClass}")
      false
  }

  def filters(k: Int): GCConnector = {
    var res: GCConnector = myInputPort3
    for (i <- 1 to k)
      res ++= filter(i.toString,(i+1).toString,is(k-i)) // fail only on the last filter
    res
  }

  val conn3 = filters(NUMFILTERS)
  val const3 = conn3.getConstraints

  println("---- filters ----")
  //  time(const2.solveXZ3)            // NOT CALLING EXTERNAL! wrong.
  time(const3.quickDataSolveSAT4J)
  time(const3.quickDataSolveZ3)
  time(const3.solveChocoDyn)
  time(const3.solveChocoPredAbstVarOrdered)



//    val sol31 = const3.solveXZ3 // buggy (not calling external)
//    println(s"sol31:\n$sol31")
//    val sol32 = const3.quickDataSolveSAT4J
//    println(s"sol32:\n$sol32")
//    val sol33 = const3.quickDataSolveZ3
//    println(s"sol33:\n$sol33")
//    val sol34 = const3.solveChocoDyn
//    println(s"sol34:\n$sol34")
//    val sol35 = const3.solveChocoPredAbstVarOrdered
//    println(s"sol35:\n$sol35")



}
