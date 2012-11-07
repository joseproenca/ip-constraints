package common.beh.guardedcommands

import common.beh.{UnFunction, UnPredicate}
import dataconnectors.ConstraintGen._

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
   af0 --(hackUser)--> af1 --[checkPwd] --> bf1 --(recallUser)--> cf1 -\
                                                                        >-> out
                       af2 --[checkPwd] --> bf2 -----------------------/
   */


  // Use "def" for different predicates: independent 'attempt' counts and buffer.
  // Use "val" for shared predicates: common attempt count and buffer.
  val secret = Map("alex" -> "123", "bob" -> "asd", "guest" -> "")

  def checkPwd = UnPredicate("chkPwd") {
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

  val hackUser = UnFunction("hackUser") {
    case x: String =>
      println("overriding username "+x)
      println("new user? (enter keeps old one)")
      lastUser = x.toString
      val p = readLine()
      if (p == "") x else p
  }


  val recallUser = UnFunction("recallUser") {
    case x:String => lastUser
  }


  val connector =
    writer("alex",List("alex")) ++
    writer("bob", List("bob")) ++
    transf("alex","a1",hackUser) ++
    transf("a2", "a3",recallUser) ++
    filter("a1", "a2",checkPwd) ++
    filter("bob", "b1",checkPwd) ++
    merger("a3","b1","out") ++
    reader("out",1) ++
    // at least one should have flow
    flow("out")


  // Run

  val sol = connector.lazyDataSolve

//  println("-----------\n" + c.commands.mkString("\n"))
//  println("-----------")


  if (sol.isDefined) print("solved CS:\n" + sol.get.pretty)
  else println("no solution")
}
