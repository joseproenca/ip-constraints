package reopp.common.guardedcommands.dataconnectors

import reopp.common.guardedcommands._
import reopp.common.guardedcommands.Var
import reopp.common.{Predicate, Utils}
import scala.collection.JavaConversions
import reopp.common

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 08/11/12
 * Time: 10:12
 * To change this template use File | Settings | File Templates.
 */
object ConnectorGen {
  def adrain(a: String, b: String) = new GCADrain(a,b)
  def fifo(a: String, b: String, data:Option[AnyRef]) = new GCFifo(a,b,data)
  def sfifo(a: String, b: String, data:Option[AnyRef]) = new GCSyncFifo(a,b,data)
  // filters!
//  def filter(a: String, b: String, g:Guard) = new GCFilter(a,b,g)
  def filter(a: String, b: String, filter:Predicate) = new GCFilter(a,b,filter)
  def filter[A](a: String, b: String, filter: (A) => Boolean) = new GCTFilter(a,b,filter)
  def negfilter(a: String, b: String, filter:Predicate) = new GCFilter(a,b,filter,false)
  def sfilter(a: String, b: String, filter:Predicate) = new GCSFilter(a,b,filter)
  /////
  def imerger(a: String, b: String, c: String) = new GCIMerger(a,b,c)
  def merger(a: String, b: String, c: String) = new GCMerger(a,b,c)
  def nmerger(srcs: List[String], snk: String) = new GCNMerger(srcs,snk)
  def lossy(a: String, b: String) = new GCLossy(a,b)
  def sdrain(a: String, b: String) = new GCSDrain(a,b)
  def sspout(a: String, b: String) = new GCSSpout(a,b)
  def sync(a: String, b: String) = new GCSync(a,b)
  // transformers!
  def transf(a: String, b: String, f: common.Function) = new GCTransf(a,b,f)
  def transf(a: String, b: String, f: common.Function, undo: common.Function) = new GCTransfUndo(a,b,f,undo)
  def transf(as: List[String], b: String, f: common.Function) = new GCNTransf(as,b,f)
  // using (scala's) partial functions
  def transf(a: String, b: String, f: PartialFunction[Any,Any]) = new GCPTransf(a,b,f)
//  def transf[A](a: String, b: String, f: PartialFunction[A,_]) = new GCPTransf(a,b,f)
  // using (scala's) total funtions
  def transf[A](a: String, b: String, f: (A) => _) = new GCTTransf[A](a,b,f)
  /////
  def monitor(a: String, b: String, f: common.Function) = new GCMonitor(a,b,f)
  def exrouter(a: String, b: String, c: String) = new GCExRouter(a,b,c)
  def nexrouter(src: String, snks: List[String]) = new GCNExRouter(src,snks)
  def reader(a: String,n: Int) = new GCReader(a,n)
  def reader(a: String) = new GCReader(a,-1)
  def writer(a: String, data: List[Any]) = new GCWriter(a,data)
  def jwriter(a: String, data: java.util.List[Any]) =
    new GCWriter(a,data.toArray.toList)

  def flow(a: String) = new GCConnector(List(a)) {
    def getConstraints = Formula(True --> a)//Var(Utils.flowVar(a,getID)))
  }
  def noflow(a: String) = new GCConnector(List(a)) {
    def getConstraints = Formula(True --> !a)//Neg(Var(Utils.flowVar(a,getID))))
  }
  def empty = new GCConnector(List()) {
    def getConstraints = Formula()
  }
}
