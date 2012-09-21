package common.beh.guardedcommands.dataconnectors

import common.beh.guardedcommands._
import common.beh.{UnPredicate, UnFunction, Utils}
import common.beh.guardedcommands.Var
import scala.Some

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 27/07/12
 * Time: 10:01
 * To change this template use File | Settings | File Templates.
 */

object ConstraintGen {
  def adrain(a: String, b: String, i: Int = 0) = new GCADrain(a,b,i).constraints
  def fifo(a: String, b: String, data:Option[Any], i: Int = 0) = new GCFifo(a,b,data,i).constraints
  def sfifo(a: String, b: String, data:Option[Any], i: Int = 0) = new GCSyncFifo(a,b,data,i).constraints
  def filter(a: String, b: String, g:Guard) = new GCFilter(a,b,0,g).constraints
  def filter(a: String, b: String, filter:UnPredicate, i: Int = 0) = new GCFilter(a,b,i,filter).constraints
  def negfilter(a: String, b: String, filter:UnPredicate, i: Int = 0) = new GCFilter(a,b,i,filter,false).constraints
  def imerger(a: String, b: String, c: String, i: Int = 0) = new GCIMerger(a,b,c,i).constraints
  def merger(a: String, b: String, c: String, i: Int = 0) = new GCMerger(a,b,c,i).constraints
  def nmerger(srcs: List[String], snk: String, i: Int = 0) = new GCNMerger(srcs,snk,i).constraints
  def lossy(a: String, b: String, i: Int = 0) = new GCLossy(a,b,i).constraints
  def sdrain(a: String, b: String, i: Int = 0) = new GCSDrain(a,b,i).constraints
  def sspout(a: String, b: String, i: Int = 0) = new GCSSpout(a,b,i).constraints
  def sync(a: String, b: String, i: Int = 0) = new GCSync(a,b,i).constraints
  def transf(a: String, b: String, f: UnFunction, i: Int = 0) = new GCTransf(a,b,i,f).constraints
  def exrouter(a: String, b: String, c: String, i: Int = 0) = new GCExRouter(a,b,c,i).constraints
  def nexrouter(src: String, snks: List[String], i: Int = 0) = new GCNExRouter(src,snks,i).constraints
  def reader(a: String,n: Int, i: Int = 0) = new GCReader(a,n,i).constraints
  def writer(a: String, data: List[Any], i: Int = 0) = new GCWriter(a,i,data).constraints

  def flow(a: String, i: Int = 0) = GuardedCommands(True --> Var(Utils.flowVar(a,i)))
  def noflow(a: String, i: Int = 0) = GuardedCommands(True --> Neg(Var(Utils.flowVar(a,i))))
}
