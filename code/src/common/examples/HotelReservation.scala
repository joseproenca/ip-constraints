package common.examples

import common.Predicate
import common.Function
import common.guardedcommands.dataconnectors.ConnectorGen._

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 15/01/13.
 */

class HotelReservation

object HotelReservation extends App {

//  implicit def scalaFunc2Func(f: Any => Any): Function = Function()(f)
//  implicit def scalaFunc2Pred(p: Any => Boolean): Predicate = Predicate()(p)

  case class Reqq(time: Int, val selection: Int)

  def booking(n:String, search:Function, select:Function, book:Function,
            cancel:Function, ok:Predicate) =
    transf(n+"_in",n+"_a",search) ++
    transf(n+"_a",n+"_b",select) ++
    transf(n+"_b",n+"_c",book,cancel) ++
    filter(n+"_c",n+"_out",ok) ++
    sdrain(n+"_c",n+"_out")


  def hotel(id:Int) = booking("h"+id,
    Function("hsr"+id){case (x:Reqq) => {println("h searching..."+id); x}},
    Function("hsl"+id){case (x:Reqq) => {println("h selecting..."+id); x}},
    Function("hb"+id){case (x:Reqq) => {println("h booking..."+id); x}},
    Function("hc"+id){case (x:Reqq) => {println("h CANCELING..."+id); x}},
    Predicate("hp"+id){case (x:Reqq) => {println("h checking..."+id);x.selection==id}}
  )

  def flight(id:Int) = booking("f"+id,
    Function("fsr"+id){case (x:Reqq) => {println("f searching..."+id); x}},
    Function("fsl"+id){case (x:Reqq) => {println("f selecting..."+id); x}},
    Function("fb"+id){case (x:Reqq) => println("f booking..."+id); x},
    Function("fc"+id){case (x:Reqq) => println("f CANCELING..."+id); x},
    Predicate("fp"+id){
      case (x:Reqq) => {
//        println("fffff - is "+x.selection+"=="+id+"? - "+(x.selection==id))
        println("f checking("+x+")..."+id)
        readLine()
        x.selection==id}
    }
  )

  val connector =
    writer("start",List(new Reqq(19,1))) ++
    exrouter("start","f1_in","f2_in") ++
    flight(1) ++ flight(2) ++
    hotel(1) ++ hotel(2) ++ hotel(3) ++
    exrouter("f1_out","h1_in","h2_in") ++
    sync("f2_out","h3_in") ++
    adrain("h1_out","h2_out")

//  println("starting")
//  val s1 = connector.step
//  println("done1 - \n"+s1)//c.getConstraints)
//  val s2 = connector.step
//  println("done2 - \n"+s2)//c.getConstraints)
//  connector.run



  ////////////////////////////////////
  // second connector, in the paper //
  ////////////////////////////////////

  case class Req(val content:String)

  // Define functions

  def srchHotel(i:Int) = Function("SearchHotel-"+i){
    case r:Req => i match {
      case 1 => List("F1","Ibis","Mercury")
      case 2 => List("B&B","YHostel")
      case _ => List("Aaa","Bbb")
    }
  }

  val approve = Predicate("approve"){
    case l:List[String] =>
      println("approve: "+l.mkString(", ")+". [y,n]")
      readLine() == "y"
    case other =>
      println("appr: strange type - "+other+" : "+other.getClass)
      false
  }

  val book = Function("book"){
    case l : List[String] =>
      println("Options: "+l.mkString(", ")+
        ". Which one? (1.."+l.length+")")
      val res = readInt()
      l((res-1)%l.length)
    case other =>
      println("book: strange type - "+other+" : "+other.getClass)
  }

  val cancelB = Function("cancelB"){
    case x => println("canceling booking "+x+".") // returns Unit
  }

  val invoice = Function("invoice"){
    case x => println("sending invoide for "+x+".") // returns Unit
  }

  val pay = Predicate("pay"){
    case x => if (x == "Ibis") {
      println("paying for Ibis")
      true
    }
    else {
      println("not paying for "+x)
      false
    }
  }

  val paid = pay

  // the connector:
  val conn2 =
    writer("req", List(Req("re1"), Req("req2"))) ++
    nexrouter("req",List("h1","h2","h3")) ++
    transf("h1","h1o",srchHotel(1)) ++
    transf("h2","h2o",srchHotel(2)) ++
    transf("h3","h3o",srchHotel(3)) ++
    nmerger(List("h1o","h2o","h3o"),"hs") ++
    filter("hs","ap",approve) ++
    sdrain("hs","ap") ++
    transf("ap","bk",book,cancelB) ++
    monitor("bk","inv",invoice) ++
    filter("inv","paid",pay) ++
    negfilter("inv","npaid",pay) ++
    reader("paid",5) ++ reader("npaid",5)

  val conn3 = writer("req",List(Req("req1"),
    Req("req2"))) ++
    nexrouter("req",List("h1","h2","h3")) ++
    transf("h1","h1o",srchHotel(1)) ++
    transf("h2","h2o",srchHotel(2)) ++
    transf("h3","h3o",srchHotel(3)) ++
    nmerger(List("h1o","h2o","h3o"),"hs") ++
    filter("hs","ap",approve) ++
    // sdrain("hs","ap") ++
    transf("ap","bk",book,cancelB) ++
    monitor("bk","inv",invoice) ++
    filter("inv","paid",paid) ++
    sdrain("hs","paid") ++
    reader("paid",5)

//  conn3.run()


  val approve2 = Predicate("approve2"){
    case l:String =>
      println("approve: "+l+". [y,n]")
      readLine() == "y"
    case other =>
      println("appr2: strange type - "+other+" : "+other.getClass)
      false
  }

  val testnewsolve = writer("a",List(List("F1","Ibis"))) ++
      transf("a","b",book) ++ nmerger(List("b","none"),"b2") ++ noflow("none") ++ filter("b2","c",approve2) ++ reader("c",1)


  val conn4 = writer("1-h",List(Req("req1"),Req("req2"))) ++
    nexrouter("1-h",List("11-h1","12-h2","13-h3")) ++
    transf("11-h1","14-h1o",srchHotel(1)) ++
    transf("12-h2","15-h2o",srchHotel(2)) ++
    transf("13-h3","16-h3o",srchHotel(3)) ++
    nmerger(List("14-h1o","15-h2o","16-h3o"),"2-ho") ++
//    transf("1-h","2-ho",srchHotel(1)) ++
    filter("2-ho","3-ap",approve) ++  sdrain("2-ho","3-ap") ++
    transf("3-ap","4-bk",book,cancelB)



  //  testnewsolve.run()
  conn2.getConstraints.solveChocoX
}
