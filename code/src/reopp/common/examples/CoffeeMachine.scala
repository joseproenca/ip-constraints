package reopp.common.examples

import reopp.common.{Predicate, Function}
import reopp.common.guardedcommands.dataconnectors.ConnectorGen._

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 31/07/13.
 */
class CoffeeMachine

object CoffeeMachine extends App {

  // attention: NEEDS to be "def" and not "val", otherwise rollback behaves unexpected:
  //  for every successful pay(x) will roll back all other attempted pay(y), even if y
  //  was successfull somewhere else.
  val pay = Function("pay") {
    case item:String =>
      println("Pay for "+item+". [y/n]")
      (item, readLine() == "y")
    case x => println("Strange (pay): "+x)
  }

  val payed = Predicate("payed") {
    case (_,b:Boolean) => b
    case x => println("Strange (payed): "+x); false
  }

  val reimburse = Function("reimburse") {
    case (in,(out1,out2:Boolean)) =>
      if (out2) println("reimbursing for "+out1)
      else println("no need to reimburse for "+out1)
  }

  val concat = Function("concat") {
    case List((i1:String,p1:Boolean),(i2:String,p2:Boolean)) => (i1 +"/"+ i2,p1 && p2)
    case x => println("Strange (concat): "+x)
  }

  val conn =
    writer("c",for (_ <- (1 to 3).toList) yield "coffee") ++ // 3 coffees
    writer("l",for (_ <- (1 to 2).toList) yield "latte") ++ // 2 lattes
    writer("m",for (_ <- (1 to 2).toList) yield "mokka") ++ // 2 mokkas
    transf("c","cp",pay,reimburse) ++ sfilter("cp","cp2",payed) ++
    transf("l","lp",pay,reimburse) ++ sfilter("lp","lp2",payed) ++
    transf("m","mp",pay,reimburse) ++ sfilter("mp","mp2",payed) ++
    merger("lp2","mp2","lmp") ++
    //
    transf(List("cp2","lmp"),"clm",concat) ++    // if binary functions are supported
//    sync("lmp","clm") ++ sdrain("cp2","lmp") ++  // if binary functions are not supported
    transf("clm","out2", (x:(String,Boolean)) => x._1 + "-AAAA-"+x._2) ++
//    transf[String]("clm","out2", (x:String) => x + "-AAAA-") ++
    reader("clm",3) ++
    reader("out2",3)

  conn.run()
}


