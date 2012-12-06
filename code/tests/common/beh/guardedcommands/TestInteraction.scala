package common.beh.guardedcommands

import dataconnectors._
import org.scalatest.FunSpec
import common.beh.Predicate

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 18/07/12
 * Time: 10:57
 * To change this template use File | Settings | File Templates.
 */

class TestInteraction extends FunSpec {

  describe ("Interaction - ask password of a single user.") {

    val checkPwd = new Predicate {
      var attempts = 3

      val secret = Map("joe" -> "asd", "alex" -> "123", "guest" -> "")

      def check(x: Any) = if (x.isInstanceOf[String]) {
        if (attempts <= 0) {
          println("No more attempts!")
          false
        }
        else {
          val user = x.asInstanceOf[String]
          if (!(secret contains user)) {
            println("Unknown user "+user)
            false
          }
          else {
            println("Password for user "+user)
            var pass = "asd" //readLine()
            if (secret(user) == pass) {
              println("# OK #")
              true
            }
            else {
              println("# FAIL #")
              attempts -= 1
              false
            }
          }
        }
      }
      else false

      override def toString = "chkPwd"
    }

    val c =
      new GCFilter("af1","bf1",0,checkPwd).getConstraints ++
      new GCFilter("af2","bf2",0,checkPwd).getConstraints ++
      new GCMerger("bf1","bf2","out",0).getConstraints ++
      new GCWriter("af1",0,List("joe")).getConstraints ++
      new GCWriter("af2",0,List("alex")).getConstraints ++
      new GCADrain("af1","af2",0).getConstraints

    // TEST
    // data fails first filter, second should be lazy using chocoSAT. No flow on "c", so fail.

    val res= c.solveChocoBool

    println("-----------\n"+c.commands.mkString("\n"))
    println("-----------")


    if (res.isDefined) print("solved:\n"+res.get.pretty)
    else println("no solution")

    if (res.isDefined) println("partial eval: "+c.partialEval(res.get))

    it ("c should have a sol") {assert (res.isDefined)}

  }

}
