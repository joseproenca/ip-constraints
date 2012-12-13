package common.guardedcommands.dataconnectors

import common.guardedcommands._
import common.guardedcommands.Var
import scala.Some
import common.{Predicate, Utils}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 27/07/12
 * Time: 10:01
 * To change this template use File | Settings | File Templates.
 */

object ConstraintGen {
  def adrain(a: String, b: String, i: Int = 0) = new GCADrain(a,b,i).getConstraints
  def fifo(a: String, b: String, data:Option[Any], i: Int = 0) = new GCFifo(a,b,data,i).getConstraints
  def sfifo(a: String, b: String, data:Option[Any], i: Int = 0) = new GCSyncFifo(a,b,data,i).getConstraints
  def filter(a: String, b: String, g:Guard) = new GCFilter(a,b,0,g).getConstraints
  def filter(a: String, b: String, filter:Predicate, i: Int = 0) = new GCFilter(a,b,i,filter).getConstraints
  def negfilter(a: String, b: String, filter:Predicate, i: Int = 0) = new GCFilter(a,b,i,filter,false).getConstraints
  def imerger(a: String, b: String, c: String, i: Int = 0) = new GCIMerger(a,b,c,i).getConstraints
  def merger(a: String, b: String, c: String, i: Int = 0) = new GCMerger(a,b,c,i).getConstraints
  def nmerger(srcs: List[String], snk: String, i: Int = 0) = new GCNMerger(srcs,snk,i).getConstraints
  def lossy(a: String, b: String, i: Int = 0) = new GCLossy(a,b,i).getConstraints
  def sdrain(a: String, b: String, i: Int = 0) = new GCSDrain(a,b,i).getConstraints
  def sspout(a: String, b: String, i: Int = 0) = new GCSSpout(a,b,i).getConstraints
  def sync(a: String, b: String, i: Int = 0) = new GCSync(a,b,i).getConstraints
  def transf(a: String, b: String, f: common.Function, i: Int = 0) = new GCTransf(a,b,i,f).getConstraints
  def monitor(a: String, b: String, f: common.Function, i: Int = 0) = new GCMonitor(a,b,i,f).getConstraints
  def exrouter(a: String, b: String, c: String, i: Int = 0) = new GCExRouter(a,b,c,i).getConstraints
  def nexrouter(src: String, snks: List[String], i: Int = 0) = new GCNExRouter(src,snks,i).getConstraints
  def reader(a: String,n: Int, i: Int = 0) = new GCReader(a,i,n).getConstraints
  def writer(a: String, data: List[Any], i: Int = 0) = new GCWriter(a,i,data).getConstraints

  def flow(a: String, i: Int = 0) = GuardedCommands(True --> Var(Utils.flowVar(a,i)))
  def noflow(a: String, i: Int = 0) = GuardedCommands(True --> Neg(Var(Utils.flowVar(a,i))))
}
