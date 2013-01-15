package common.guardedcommands

import dataconnectors.ConnectorGen._
import common.Predicate
import common.Function

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 15/01/13.
 */

class HotelReservation

object HotelReservation extends App {

  implicit def scalaFunc2Func(f: Any => Any): Function = Function()(f)
  implicit def scalaFunc2Pred(p: Any => Boolean): Predicate = Predicate()(p)

  case class Req(time: Int, val selection: Int)

  def booking(n:String, search:Function, select:Function, book:Function,
            cancel:Function, ok:Predicate) =
    transf(n+"_in",n+"_a",search) ++
    transf(n+"_a",n+"_b",select) ++
    transf(n+"_b",n+"_c",book,cancel) ++
    filter(n+"_c",n+"_out",ok) ++
    sdrain(n+"_c",n+"_out")


  def hotel(id:Int) = booking("h"+id,
    Function("hsr"+id){case (x:Req) => {println("h searching..."+id); x}},
    Function("hsl"+id){case (x:Req) => {println("h selecting..."+id); x}},
    Function("hb"+id){case (x:Req) => {println("h booking..."+id); x}},
    Function("hc"+id){case (x:Req) => {println("h CANCELING..."+id); x}},
    Predicate("hp"+id){case (x:Req) => {println("h checking..."+id);x.selection==id}}
  )

  def flight(id:Int) = booking("f"+id,
    Function("fsr"+id){case (x:Req) => {println("f searching..."+id); x}},
    Function("fsl"+id){case (x:Req) => {println("f selecting..."+id); x}},
    Function("fb"+id){case (x:Req) => println("f booking..."+id); x},
    Function("fc"+id){case (x:Req) => println("f CANCELING..."+id); x},
    Predicate("fp"+id){
      case (x:Req) => {
//        println("fffff - is "+x.selection+"=="+id+"? - "+(x.selection==id))
        println("f checking("+x+")..."+id)
        readLine()
        x.selection==id}
    }
  )

  val connector =
    writer("start",List(new Req(19,1))) ++
    exrouter("start","f1_in","f2_in") ++
    flight(1) ++ flight(2) ++
    hotel(1) ++ hotel(2) ++ hotel(3) ++
    exrouter("f1_out","h1_in","h2_in") ++
    sync("f2_out","h3_in") ++
    adrain("h1_out","h2_out")

  println("starting")
  val s1 = connector.step
  println("done1 - \n"+s1)//c.getConstraints)
  val s2 = connector.step
  println("done2 - \n"+s2)//c.getConstraints)
//  connector.run

}
