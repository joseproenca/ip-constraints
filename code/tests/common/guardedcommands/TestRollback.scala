package common.guardedcommands

import org.scalatest.FunSpec
import common.{Connector, Function, Predicate}
import common.guardedcommands.dataconnectors.ConnectorGen._
import common.Utils.flowVar

/**
 * Test if the rollback function is exectuted.
 * Only applies when there is an external function calculated during constraint solving:
 * solveChocoX, solveChocoDyn, lazyDataSolve
 *
 * Created by jose on 15/04/13.
 */
class TestRollback extends FunSpec {

  describe ("Rollback (chocoDyn) test -") {
    // STATE
    var counter_asked = 0
    var fstRequest = "a"
    var counter_cancel = 0
    var request_cancel = "b"

    // asked once: give "1", asked twice: give "2"
    val srchHotel = Function("SearchHotel"){
      case input:String=>
//        println("searching hotel, counter "+counter_asked)
        counter_asked += 1
        counter_asked match {
          case 1 =>
            fstRequest = input
            List("B&B","YHostel")
          case 2 =>
            List("F1","Ibis","Mercury")
        }
    }

    val cancelSrch = Function("cancelSrch"){
      case x: String => {counter_cancel+=1; request_cancel = x}
    }

    val hasIbis = Predicate("hasIbis"){
      case l:List[String] =>
//        println(l.toString + " has ibis?")
        l contains "Ibis"
    }

    val conn =
      writer("req1",List("r1","r3")) ++
      writer("req2",List("r2","r4")) ++
      merger("req1","req2","req") ++
      transf("req","list",srchHotel,cancelSrch) ++
      filter("list","res",hasIbis) ++
      sdrain("list","res")
//      reader("res")

//    println(conn.getConstraints)
    val sol = conn.getConstraints.solveChocoDyn
//    println(sol)
    conn.update(sol)

    it ("dyn should cancel once") {assert(counter_cancel == 1)}
    it ("dyn should cancel first request") {fstRequest == request_cancel}
    it ("dyn should have dataflow") {sol.isDefined}
    it ("dyn should have dataflow on output") {sol.get.hasFlowOn(flowVar("res"))}

//    //// OTHER SOLVERS ////
//    counter_asked = 0
//    fstRequest = ""
//    counter_cancel = 0
//    request_cancel = ""
//
//    val sol2 = conn.getConstraints.solveChocoSat
//    conn.update(sol2)
//
//    println("cancelled: " +counter_cancel)
//
//    it ("sat should cancel once") {assert(counter_cancel == 1)}
//    it ("sat should cancel first request") {fstRequest == request_cancel}
//    it ("sat should have dataflow") {sol2.isDefined}
//    it ("sat should have dataflow on output") {sol2.get.hasFlowOn(flowVar("res"))}
  }


  describe ("Rollback (Choco lazy) test -") {
    // STATE
    var counter_asked = 0
    var fstRequest = "a"
    var counter_cancel = 0
    var request_cancel = "b"

    // asked once: give "1", asked twice: give "2"
    val srchHotel = Function("SearchHotel"){
      case input:String=>
        //        println("searching hotel, counter "+counter_asked)
        counter_asked += 1
        counter_asked match {
          case 1 =>
            fstRequest = input
            List("B&B","YHostel")
          case 2 =>
            List("F1","Ibis","Mercury")
        }
    }

    val cancelSrch = Function("cancelSrch"){
      case x: String => {counter_cancel+=1; request_cancel = x}
    }

    val hasIbis = Predicate("hasIbis"){
      case l:List[String] =>
        //        println(l.toString + " has ibis?")
        l contains "Ibis"
    }

    val conn: Connector[GCSolution,Formula] =
      writer("req1",List("r1","r3")) ++
      writer("req2",List("r2","r4")) ++
      merger("req1","req2","req") ++
      transf("req","list",srchHotel,cancelSrch) ++
      filter("list","res",hasIbis) ++
      sdrain("list","res")
    //      reader("res")

    val sol2 = conn.getConstraints.lazyDataSolve
    conn.update(sol2)

//    println("cancelled: " +counter_cancel)

    it ("sat should cancel once") {assert(counter_cancel == 1)}
    it ("sat should cancel first request") {fstRequest == request_cancel}
    it ("sat should have dataflow") {sol2.isDefined}
    it ("sat should have dataflow on output") {sol2.get.hasFlowOn(flowVar("res"))}


  }


  describe ("Rollback (ChocoX) test -") {
    // STATE
    var counter_asked = 0
    var fstRequest = "a"
    var counter_cancel = 0
    var request_cancel = "b"

    // asked once: give "1", asked twice: give "2"
    val srchHotel = Function("SearchHotel"){
      case input:String=>
        //        println("searching hotel, counter "+counter_asked)
        counter_asked += 1
        counter_asked match {
          case 1 =>
            fstRequest = input
            List("B&B","YHostel")
          case 2 =>
            List("F1","Ibis","Mercury")
        }
    }

    val cancelSrch = Function("cancelSrch"){
      case x: String => {counter_cancel+=1; request_cancel = x}
    }

    val hasIbis = Predicate("hasIbis"){
      case l:List[String] =>
        //        println(l.toString + " has ibis?")
        l contains "Ibis"
    }

    val conn: Connector[GCSolution,Formula] =
      writer("req1",List("r1","r3")) ++
        writer("req2",List("r2","r4")) ++
        merger("req1","req2","req") ++
        transf("req","list",srchHotel,cancelSrch) ++
        filter("list","res",hasIbis) ++
        sdrain("list","res")
    //      reader("res")

    val sol3 = conn.getConstraints.solveChocoX
    conn.update(sol3)

    //    println("cancelled: " +counter_cancel)

    it ("sat should cancel once") {assert(counter_cancel == 1)}
    it ("sat should cancel first request") {fstRequest == request_cancel}
    it ("sat should have dataflow") {sol3.isDefined}
    it ("sat should have dataflow on output") {sol3.get.hasFlowOn(flowVar("res"))}


  }

}
