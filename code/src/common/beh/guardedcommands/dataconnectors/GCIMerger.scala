package common.beh.guardedcommands.dataconnectors

import common.beh.guardedcommands._
import common.beh.Utils._
import common.beh.guardedcommands.Neg
import common.beh.guardedcommands.VarAssgn
import common.beh.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 19/06/12
 * Time: 21:08
 * To change this template use File | Settings | File Templates.
 */

class GCIMerger(a: String, b: String, c: String, uid: Int) extends GCConnector(List(a,b,c), uid) {
  val av = Var(flowVar(a,uid))
  val bv = Var(flowVar(b,uid))
  val cv = Var(flowVar(c,uid))
  val abv = Var(flowVar(a+b,uid))

  var constraints = GuardedCommands(Set(
    cv --> (av or bv),
    (av or bv) --> cv
  ))

  if (useData) constraints ++= Set(
    (av and bv and abv)      --> VarAssgn(dataVar(c,uid),dataVar(a,uid)),
    (av and bv and Neg(abv)) --> VarAssgn(dataVar(c,uid),dataVar(b,uid)),
    (av and Neg(bv)) --> VarAssgn(dataVar(c,uid),dataVar(a,uid)),
    (bv and Neg(av)) --> VarAssgn(dataVar(c,uid),dataVar(b,uid))
  )
}