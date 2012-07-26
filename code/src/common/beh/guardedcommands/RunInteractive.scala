package common.beh.guardedcommands

import common.beh.choco.genericconstraints.{UnFunction, UnPredicate}
import dataconnectors._
import common.beh.Utils._

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 11:28
 * To change this template use File | Settings | File Templates.
 */

object Main extends App {

  /*
   af0 --(hackUser)--> af1 --[checkPwd] --> bf1 --(recallUser)--> cf1 -_
                                                                        >-> out
                       af2 --[checkPwd] --> bf2 -----------------------'
   */


  // Use "def" for different predicates: independent 'attempt' counts and buffer.
  // Use "val" for shared predicates: common attempt count and buffer.
  def checkPwd = new UnPredicate {
    var attempts = 2

    val secret = Map("joe" -> "asd", "alex" -> "123", "guest" -> "")

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
            check(x)
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



  val c =
    new GCTransf("af0", "af1", 0, hackUser).constraints ++
      new GCFilter("af1", "bf1", 0, checkPwd).constraints ++
      new GCTransf("bf1", "cf1", 0, recallUser).constraints ++
      new GCFilter("af2", "bf2", 0, checkPwd).constraints ++
      new GCMerger("cf1", "bf2", "out", 0).constraints ++
      new GCWriterData("af0", 0, List("joe")).constraints ++
      new GCWriterData("af2", 0, List("alex")).constraints ++
      // at least one should have flow
//      new GCMerger("af1","af2","forceFlow",0).constraints ++
//      new GCSDrain("forceFlow","out",0).constraints
      // other experiments
//      new GCADrain("af1", "af2", 0).constraints
//      GuardedCommands(True --> SGuard(Neg(Var(flowVar("bf1",0)))))
      GuardedCommands(True --> SGuard(Var(flowVar("out",0))))

  // Run

  val res = c.lazyDataSolve

//  println("-----------\n" + c.commands.mkString("\n"))
//  println("-----------")


  if (res.isDefined) print("solved CS:\n" + res.get.pretty)
  else println("no solution")

//  if (res.isDefined) println("partial eval: " + c.partialEval(res.get))
}
