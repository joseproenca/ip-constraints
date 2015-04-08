package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.guardedcommands.Var
import scala.Some
import reopp.common.{Predicate, IntPredicate, Utils}

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 27/07/12
 * Time: 10:01
 * To change this template use File | Settings | File Templates.
 */

object ConstraintGen {
  def adrain(a: String, b: String) = new GCADrain(a,b).getConstraints
  def fifo(a: String, b: String, data:Option[Any]) = new GCFifo(a,b,data).getConstraints
  def sfifo(a: String, b: String, data:Option[Any]) = new GCSyncFifo(a,b,data).getConstraints
//  def filter(a: String, b: String, g:Guard) = new GCFilter(a,b,0,g).getConstraints
  def ifilter(a: String, b: String, filter:IntPredicate) = new GCIFilter(a,b,filter).getConstraints
  def inegfilter(a: String, b: String, filter:IntPredicate) = new GCIFilter(a,b,filter,false).getConstraints
  def filter(a: String, b: String, filter:Predicate) = new GCFilter(a,b,filter).getConstraints
  def negfilter(a: String, b: String, filter:Predicate) = new GCFilter(a,b,filter,false).getConstraints
  def imerger(a: String, b: String, c: String) = new GCIMerger(a,b,c).getConstraints
  def genfilter(a: String, b: String, guardFunc: Var=>Guard) = new GCGenFilter(a,b,guardFunc).getConstraints
  def merger(a: String, b: String, c: String) = new GCMerger(a,b,c).getConstraints
  def nmerger(srcs: List[String], snk: String) = new GCNMerger(srcs,snk).getConstraints
  def lossy(a: String, b: String) = new GCLossy(a,b).getConstraints
  def sdrain(a: String, b: String) = new GCSDrain(a,b).getConstraints
  def sspout(a: String, b: String) = new GCSSpout(a,b).getConstraints
  def sync(a: String, b: String) = new GCSync(a,b).getConstraints
  def transf(a: String, b: String, f: reopp.common.Function) = new GCTransf(a,b,f).getConstraints
  def monitor(a: String, b: String, f: reopp.common.Function) = new GCMonitor(a,b,f).getConstraints
  def exrouter(a: String, b: String, c: String) = new GCExRouter(a,b,c).getConstraints
  def nexrouter(src: String, snks: List[String]) = new GCNExRouter(src,snks).getConstraints
  def reader(a: String,n: Int) = new GCReader(a,n).getConstraints
  def writer(a: String, data: List[Any]): Formula = new GCWriter(a,data).getConstraints

  def flow(a: String) = Formula(True --> Var(Utils.mkFlowVar(a)))
  def noflow(a: String) = Formula(True --> Neg(Var(Utils.mkFlowVar(a))))
}
