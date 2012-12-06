package common.beh.guardedcommands

import common.beh.{Function, Predicate}
import dataconnectors.ConnectorGen._
import workers.{Deployer, Node}
import workers.strategies.HybridStrategy
import workers.strategies.HybridStrategy.HybridStrategyBuilder
import GCConnector.GCBuilder
import common.beh.guardedcommands.{GuardedCommands => C,GCSolution => S}
import common.beh.Utils._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:28
 * To change this template use File | Settings | File Templates.
 */

class RunInteractive

object RunInteractive extends App {

  /*
   af0 --(hackUser)--> af1 --[checkPwd] --> bf1 --(recallUser)--> cf1 -_
                                                                        >-> out
                       af2 --[checkPwd] --> bf2 -----------------------
   */


  // Use "def" for different predicates: independent 'attempt' counts and buffer.
  // Use "val" for shared predicates: common attempt count and buffer.
  val secret = Map("alex" -> "123", "bob" -> "asd", "guest" -> "")

  def checkPwd = Predicate("chkPwd") {
    case user: String =>
      var attempts = 2
      var succeed = false

      if (secret contains user)
        while (attempts > 0) {
          println("Password for user " + user + " ("+attempts+" attempts)")
          var pass = readLine()
          if (secret(user) == pass) {
            println("#### OK ####")
            succeed = true
            attempts = 0
          }
          attempts -= 1
        }
      else println("Unknown user " + user)
      succeed
  }



  // Replacing temporarily the user name for authentication
  var lastUser = ""

  val hackUser = Function("hackUser") {
    case x: String =>
      println("overriding username "+x)
      println("new user? (enter keeps old one)")
      lastUser = x.toString
      val p = readLine()
      if (p == "") x else p
  }

  val undohack = Function("undo") {
    x => println("-- hack on "+x+" not used --")
  }

  val recallUser = Function("recallUser") {
    case x:String => lastUser
  }


  val connector =
    writer("alex",List("alex")) ++
    writer("bob", List("bob")) ++
    transf("alex","a1",hackUser,undohack) ++
    transf("a2", "a3",recallUser) ++
    filter("a1", "a2",checkPwd) ++
    filter("bob", "b1",checkPwd) ++
    merger("a3","b1","out") ++
    reader("out",2) ++
    // at least one should have flow
    flow("out")


//  // create and run deployer
//  val deployer = new Deployer[S,C,HybridStrategy[S,C]](2)
//  deployer.start()
//
//  val node_a = Node[S,C](deployer, (uid:Int) =>
//    writer("alex",List("alex"),uid) ++
//    transf("alex","a1",hackUser,undohack,uid) ++
//    filter("a1", "a2",checkPwd,uid) ++
//    transf("a2", "a3",recallUser,uid)
//  , "a3" -> "alex" // if "a3" has flow, check if "alex" also has...
//  )
//  val node_b = Node[S,C](deployer, (uid:Int) =>
//    writer("bob", List("bob"),uid) ++
//    filter("bob", "b1",checkPwd,uid)
//  , "b1" -> "bob"
//  )
//  val node_c = Node[S,C](deployer, (uid:Int) =>
//    merger("a3","b1","out",uid) ++
//    reader("out",2,uid) ++
//    flow("out",uid)
//  , "out" -> "a3"
//  )
//
//  node_c("a3") <-- node_a("a3")
//  node_c("b1") <-- node_b("b1")
//
//  node_c.init()
//  node_b.init()
//  node_a.init()


  // Run

  val sol = connector.step // getConstraints.lazyDataSolve

//  println("-----------\n" + c.commands.mkString("\n"))
//  println("-----------")


//  if (sol.isDefined) print("solved CS:\n" + sol.get.pretty)
  if (sol.isDefined) println("-- data through 'out': " +
    (sol.get getDataOn dataVar("out")).get+" --")
  else {
    println("no solution")
    sys.exit()
  }

//  connector.update(sol.get)
  val sol2 = connector.step  // getConstraints.lazyDataSolve

//  if (sol2.isDefined) println("solved again CS:\n" + sol2.get.pretty)
  if (sol2.isDefined) println("-- data through 'out': " +
    (sol2.get getDataOn dataVar("out")).get+" --")
  else println("no solution this time")

}
