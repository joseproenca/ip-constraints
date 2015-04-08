Examples of connectors
======================

This directory contains several examples of interactive partial connectors.
Some are simply experimenting different syntaxes.
For example, in HotelReservation we encode a toy example of a connector that coordinates a request for booking a hotel with a confirmation component, where the channels include the functionality for handeling the requests and interacting with search engines and booking services. All this functionality is invoked _during_ constraint solving.

```scala
object HotelReservation extends App {
  case class Req(val content:String)

  ...
  
    // Connector definition
  val connector =
    writer("req",List(Req("req1"), Req("req2"))) ++
    nexrouter("req",List("s1","s2","s3")) ++
    transf("s1","s1o",srchHotel(1)) ++
    transf("s2","s2o",srchHotel(2)) ++
    transf("s3","s3o",srchHotel(3)) ++
    nmerger(List("s1o","s2o","s3o"),"s") ++
    filter("s","ap",approve) ++
    sdrain("s","paid") ++
    transf("ap","bk",book,cancelB) ++
    monitor("bk","inv",invoice) ++
    filter("inv","paid",paid) ++
    reader("paid",5)

  connector.run()
}
```
Running the connector (in the last statement) will search for a solution for the coordination constraints of the connector, update the state of the connector based on this solution, and repeat the process until no solution with dataflow is found.

In the missing part we define the functionality of functions and predicates used by the connector:

```scala
  def srchHotel(i:Int) = Function("SearchHotel-"+i){
    case r:Req => i match {
      case 1 => List("F1","Ibis","Mercury")
      case 2 => List("B&B","YHostel")
      case _ => List("HotelA","HotelB")
    }
  }
  def approve = Predicate("approve"){
    case l:List[String] =>
      println("approve: "+l.mkString(", ")+". [y,n]")
      readChar() == 'y'
  }
  def book = Function("book"){
    case l : List[String] =>
      println("Options: "+l.mkString(", ")+
        ". Which one? (1.."+l.length+")")
      val res = readInt()$
      l(res-1)
  }
  def cancelB = Function("cancelB"){
    case x => println("canceling "+x+".")
  }
  def invoice = Function("invoice"){
    case x => println("invoice for "+x+".")
  }
  def pay = Predicate("paid"){
    case x => if (x == "Ibis") {
      println("paid for Ibis")
      true
    }
    else {
      println("not paid for "+x)
      false
    }
  }
```

To search for solutions concurrently, using partial constraints, one needs to define the bounds of the connectors used to search for solutions. A possible concurrent definition of this connector is defined below.

```scala
  val deployer = GenDeployer.oneStep(2)
                 // up to 2 workers
  deployer.start()

  val node_w = deployer.add {
    writer("req",List(
      Req("req1"),Req("req2"))) ++
    nexrouter("req",List("s1","s2","s3")) ++
    srchApp(1) ++ srchApp(2) ++ srchApp(3)
  }

  val node_1 = row(1)

  val node_2 = row(2)

  val node_3 = row(3)

  def srchApp(i:Int) =
    transf("s"+i,"so"+i,srchHotel(i)) ++
    filter("so"+i,"ap"+i,approve) ++
    sdrain("so"+i,"ap"+i)
  def row(i:Int) = deployer.add {
    transf("ap"+i,"bk"+i,book,cancelB) ++
    monitor("bk"+i,"inv"+i,invoice) ++
    filter("inv"+i,"paid"+i,paid) ++
    sdrain("inv"+i,"paid"+i) ++
    reader("paid"+i,5)
  }
  // connecting ports from different nodes
  node_1("ap1")  <--  node_w("ap1")
  node_2("ap2")  <--  node_w("ap2")
  node_3("ap3")  <--  node_w("ap3")

  // starting the algorithm
  deployer.init
```