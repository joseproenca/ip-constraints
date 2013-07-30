package common.guardedcommands.dataconnectors

import common.guardedcommands._
import common.guardedcommands.Var
import common.{Predicate, Utils}
import scala.collection.JavaConversions

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 08/11/12
 * Time: 10:12
 * To change this template use File | Settings | File Templates.
 */
object ConnectorGen {
  def adrain(a: String, b: String, i: Int = 0) = new GCADrain(a,b,i)
  def fifo(a: String, b: String, data:Option[AnyRef], i: Int = 0) = new GCFifo(a,b,data,i)
  def sfifo(a: String, b: String, data:Option[AnyRef], i: Int = 0) = new GCSyncFifo(a,b,data,i)
  def filter(a: String, b: String, g:Guard) = new GCFilter(a,b,0,g)
  def filter(a: String, b: String, filter:Predicate, i: Int = 0) = new GCFilter(a,b,i,filter)
  def negfilter(a: String, b: String, filter:Predicate, i: Int = 0) = new GCFilter(a,b,i,filter,false)
  def sfilter(a: String, b: String, filter:Predicate, i: Int = 0) = new GCSFilter(a,b,i,filter)
  def imerger(a: String, b: String, c: String, i: Int = 0) = new GCIMerger(a,b,c,i)
  def merger(a: String, b: String, c: String, i: Int = 0) = new GCMerger(a,b,c,i)
  def nmerger(srcs: List[String], snk: String, i: Int = 0) = new GCNMerger(srcs,snk,i)
  def lossy(a: String, b: String, i: Int = 0) = new GCLossy(a,b,i)
  def sdrain(a: String, b: String, i: Int = 0) = new GCSDrain(a,b,i)
  def sspout(a: String, b: String, i: Int = 0) = new GCSSpout(a,b,i)
  def sync(a: String, b: String, i: Int = 0) = new GCSync(a,b,i)
  def transf(a: String, b: String, f: common.Function) = new GCTransf(a,b,0,f)
  def transf(a: String, b: String, f: common.Function, i: Int) = new GCTransf(a,b,i,f)
  def transf(a: String, b: String, f: common.Function, undo: common.Function, i: Int = 0) = new GCTransfUndo(a,b,i,f,undo)
  def transf(as: List[String], b: String, f: common.Function) = new GCNTransf(as,b,0,f)
  def transf(as: List[String], b: String, f: common.Function, i: Int) = new GCNTransf(as,b,i,f)
  def transf(a: String, b: String, f: PartialFunction[Any,Any], i: Int) = new GCPTransf(a,b,i,f)
  def transf[A](a: String, b: String, f: PartialFunction[A,_]) = new GCPTransf(a,b,0,f)
  def monitor(a: String, b: String, f: common.Function, i: Int = 0) = new GCMonitor(a,b,i,f)
  def exrouter(a: String, b: String, c: String, i: Int = 0) = new GCExRouter(a,b,c,i)
  def nexrouter(src: String, snks: List[String], i: Int = 0) = new GCNExRouter(src,snks,i)
  def reader(a: String,n: Int, i: Int = 0) = new GCReader(a,i,n)
  def reader(a: String) = new GCReader(a,0,-1)
  def writer(a: String, data: List[Any], i: Int = 0) = new GCWriter(a,i,data)
  def jwriter(a: String, data: java.util.List[Any], i: Int = 0) =
    new GCWriter(a,i,data.toArray.toList)

  def flow(a: String, i: Int = 0) = new GCConnector(List(a),i) {
    def getConstraints = Formula(True --> Var(Utils.flowVar(a,i)))
  }
  def noflow(a: String, i: Int = 0) = new GCConnector(List(a),i) {
    def getConstraints = Formula(True --> Neg(Var(Utils.flowVar(a,i))))
  }
}
