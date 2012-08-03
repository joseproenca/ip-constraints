package common.beh.guardedcommands

import common.beh.choco.genericconstraints.{UnFunction, UnPredicate}
import dataconnectors._
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
   af0 --(hackUser)--> af1 --[checkPwd] --> bf1 --(recallUser)--> cf1 -_
                                                                        >-> out
                       af2 --[checkPwd] --> bf2 -----------------------'
   */


  // Use "def" for different predicates: independent 'attempt' counts and buffer.
  // Use "val" for shared predicates: common attempt count and buffer.
  def checkPwd = new UnPredicate {
    var attempts = 2

    val secret = Map("bob" -> "asd", "alex" -> "123", "guest" -> "")

    def check(x: Any) = if (x.isInstanceOf[String]) {
      val user = x.asInstanceOf[String]
      if (attempts <= 0) {
        println("No more attempts!")
        false
      }
      else if (!(secret contains user)) {
        println("Unknown user " + user)
        false
      }
      else {
        println("Password for user " + user + " ("+attempts+" attempts)")
        var pass = readLine()
        if (secret(user) == pass) {
          println("#### OK ####")
          true
        }
        else {
          attempts -= 1
          if (attempts <= 0) {
            println("#### FAIL ####")
            false
          }
          else
            check(x) // try again
        }
      }
    }
    else false

    override def toString = "chkPwd"
  }


  val hackUser = new UnFunction {
    var lastUser = ""
    def calculate(x: Any) = if (x.isInstanceOf[String]) {
      println("hacking username... requested password for user "+x)
      println("new user? (enter keeps old one)")
      lastUser = x.toString
      val p = readLine()
      if (p == "") x
      else p
    }
    override def toString = "hackUser"
  }


  val recallUser = new UnFunction {
    def calculate(x: Any) = if (x.isInstanceOf[String]) {
      hackUser.lastUser
    }
    override def toString = "recallUser"
  }


  val c = transf("aaf1","af1",hackUser) ++
    transf("bf1", "bbf1",recallUser) ++
    filter("af1", "bf1",checkPwd) ++
    filter("af2", "bf2",checkPwd) ++
    merger("bbf1","bf2","out") ++
    writer("aaf1",List("alex")) ++
    writer("af2", List("bob")) ++
    // at least one should have flow
    merger("af1","af2","forceFlow") ++
    sdrain("forceFlow","out")
    // other experiments
//    adrain("af1", "af2")
//    flow("bf1")
//    flow("out")



  // Run

  val res = c.lazyDataSolve

//  println("-----------\n" + c.commands.mkString("\n"))
//  println("-----------")


  if (res.isDefined) print("solved CS:\n" + res.get.pretty)
  else println("no solution")

//  if (res.isDefined) println("partial eval: " + c.partialEval(res.get))
}
