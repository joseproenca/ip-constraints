package common.beh.guardedcommands.dataconnectors

import common.beh.guardedcommands._
import common.beh.{Utils, Function, Predicate}
import common.beh.guardedcommands.Var

/**
 * Created with IntelliJ IDEA.
 * User: jose
 * Date: 08/11/12
 * Time: 10:12
 * To change this template use File | Settings | File Templates.
 */
object ConnectorGen {
  def adrain(a: String, b: String, i: Int = 0) = new GCADrain(a,b,i)
  def fifo(a: String, b: String, data:Option[Any], i: Int = 0) = new GCFifo(a,b,data,i)
  def sfifo(a: String, b: String, data:Option[Any], i: Int = 0) = new GCSyncFifo(a,b,data,i)
  def filter(a: String, b: String, g:Guard) = new GCFilter(a,b,0,g)
  def filter(a: String, b: String, filter:Predicate, i: Int = 0) = new GCFilter(a,b,i,filter)
  def negfilter(a: String, b: String, filter:Predicate, i: Int = 0) = new GCFilter(a,b,i,filter,false)
  def imerger(a: String, b: String, c: String, i: Int = 0) = new GCIMerger(a,b,c,i)
  def merger(a: String, b: String, c: String, i: Int = 0) = new GCMerger(a,b,c,i)
  def nmerger(srcs: List[String], snk: String, i: Int = 0) = new GCNMerger(srcs,snk,i)
  def lossy(a: String, b: String, i: Int = 0) = new GCLossy(a,b,i)
  def sdrain(a: String, b: String, i: Int = 0) = new GCSDrain(a,b,i)
  def sspout(a: String, b: String, i: Int = 0) = new GCSSpout(a,b,i)
  def sync(a: String, b: String, i: Int = 0) = new GCSync(a,b,i)
  def transf(a: String, b: String, f: Function) = new GCTransf(a,b,0,f)
  def transf(a: String, b: String, f: Function, i: Int) = new GCTransf(a,b,i,f)
  def transf(a: String, b: String, f: Function, undo: Function, i: Int = 0) = new GCTransfUndo(a,b,i,f,undo)
  def exrouter(a: String, b: String, c: String, i: Int = 0) = new GCExRouter(a,b,c,i)
  def nexrouter(src: String, snks: List[String], i: Int = 0) = new GCNExRouter(src,snks,i)
  def reader(a: String,n: Int, i: Int = 0) = new GCReader(a,i,n)
  def writer(a: String, data: List[Any], i: Int = 0) = new GCWriter(a,i,data)

  def flow(a: String, i: Int = 0) = new GCConnector(List(a),i) {
    def getConstraints = GuardedCommands(True --> Var(Utils.flowVar(a,i)))
  }
  def noflow(a: String, i: Int = 0) = new GCConnector(List(a),i) {
    def getConstraints = GuardedCommands(True --> Neg(Var(Utils.flowVar(a,i))))
  }
}
