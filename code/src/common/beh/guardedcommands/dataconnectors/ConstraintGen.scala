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
  def adrain(a: String, b: String) = new GCADrain(a,b,0).constraints
  def fifo(a: String, b: String, data:Some[Any]) = new GCFifo(a,b,data,0).constraints
  def sfifo(a: String, b: String, data:Some[Any]) = new GCSyncFifo(a,b,data,0).constraints
  def filter(a: String, b: String, g:Guard) = new GCFilter(a,b,0,g).constraints
  def filter(a: String, b: String, filter:UnPredicate) = new GCFilter(a,b,0,filter).constraints
  def negfilter(a: String, b: String, filter:UnPredicate) = new GCFilter(a,b,0,filter,false).constraints
  def imerger(a: String, b: String, c: String) = new GCIMerger(a,b,c,0).constraints
  def merger(a: String, b: String, c: String) = new GCMerger(a,b,c,0).constraints
  def nmerger(srcs: List[String], snk: String) = new GCNMerger(srcs,snk,0).constraints
  def lossy(a: String, b: String) = new GCLossy(a,b,0).constraints
  def sdrain(a: String, b: String) = new GCSDrain(a,b,0).constraints
  def sspout(a: String, b: String) = new GCSSpout(a,b,0).constraints
  def sync(a: String, b: String) = new GCSync(a,b,0).constraints
  def transf(a: String, b: String, f: UnFunction) = new GCTransf(a,b,0,f).constraints
  def exrouter(a: String, b: String, c: String) = new GCExRouter(a,b,c,0).constraints
  def nexrouter(src: String, snks: List[String]) = new GCNExRouter(src,snks,0).constraints
  def reader(a: String,n: Int) = new GCReader(a,n,0).constraints
  def writer(a: String, data: List[Any]) = new GCWriterData(a,0,data).constraints

  def flow(a: String) = GuardedCommands(True --> Var(Utils.flowVar(a,0)))
  def noflow(a: String) = GuardedCommands(True --> Neg(Var(Utils.flowVar(a,0))))
}
