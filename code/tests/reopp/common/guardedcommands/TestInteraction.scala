package reopp.common.guardedcommands

import dataconnectors._
import org.scalatest.FunSpec
import reopp.common.Predicate
import reopp.common.guardedcommands.dataconnectors.{GCADrain, GCWriter, GCMerger, GCFilter}

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
            var pass = "123" //readLine()
            if (secret(user) == pass) {
              println("# OK #")
              true
            }
            else {
              println("# FAIL #")
              attempts -= 1
              false
            }
//            true // FORCE PREDICATE TO ALWAYS SAY TRUE!
          }
        }
      }
      else false

      override def toString = "chkPwd"
    }

    val c =
      new GCFilter("af1","bf1",checkPwd) ++
      new GCFilter("af2","bf2",checkPwd) ++
      new GCMerger("bf1","bf2","out") ++
      new GCWriter("af1",List("joe")) ++
      new GCWriter("af2",List("alex")) ++
      new GCADrain("af1","af2")

    // TEST
    // data fails first filter, second should be lazy using chocoSAT. No flow on "c", so fail.

    val cs = c.getConstraints
    val res= cs.lazyDataSolve  //solveChocoBool
    val res2= cs.solveChocoX

    println("-----------\n"+cs.commands.mkString("\n"))
    println("-----------")


    if (res.isDefined) print("solved:\n"+res.get)
    else println("no solution")

    if (res.isDefined) println("partial eval: "+cs.partialEval(res.get))

    if (res.isDefined) print("X solved:\n"+res2.get)
    else println("no X solution")

    it ("c  should have a sol") {assert (res.isDefined)}
    it ("cX should have a sol") {assert (res2.isDefined)}

  }

}
