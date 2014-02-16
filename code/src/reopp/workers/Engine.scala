package reopp.workers

import akka.actor.ActorSystem
import akka.actor.Props
import reopp.common.Connector
import strategies.{Strategy, StrategyBuilder}
import reopp.common.{Constraints, Solution, CBuilder}
import reopp.common.Connector


/**
 * Manages the creation of nodes, and the creation of the main Deployer actor.
 * Starts the system by feeding all nodes to the Deployer.
 */
class Engine[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
      (maxWorkers: Int)
      (implicit builder: CBuilder[S,C], sb: StrategyBuilder[S,C,Str]) {

//	[S<:Solution,C<:Constraints[S,C],Str<:Strategy[S,C,Str]]
//  (maxWorkers: Int)
//  (implicit builder: CBuilder[S,C], sb: StrategyBuilder[S,C,Str])
	
  private val system = ActorSystem("System")
//  private val conflictManager =
//    system.actorOf(Props[ConflictManager], name = "conflictMng")

  val deployer = 
//    system.actorOf(Props(new Deployer[S,C,Str](maxWorkers,conflictManager)(builder,sb)), name = "deployer")
    system.actorOf(Props(new Deployer[S,C,Str](maxWorkers)(builder,sb)), name = "deployer")
  	
  private var nodes: List[Node[S,C]] = Nil
  
  /** Creates a new worker (node), associated to this deployer.
   *  Keeps track of created nodes just to allow starting all of them in one go. */
  def add(con: => Connector[S,C]): Node[S,C] = {
    val res = Node[S,C]((uid:Int) => con )(builder)
    nodes ::= res
    res
  }

  /** Creates a new worker (node), associated to this deployer.
   *  Keeps track of created nodes just to allow starting all of them in one go.
   *  @param deps pairs of dependent port names, Used for hybrid strategy ([[strategies.HybridStrategy]]).
   *         For each (a,b), if 'a' is not on the border of the region, 'b' cannot be either.
   */
  def add(con: => Connector[S,C],deps: Iterable[(String,String)], prior:Iterable[String]): Node[S,C] = {
    val res = Node[S,C](deps, prior, (uid:Int) => con)(builder)
    nodes ::= res
    res
  }

  /** Starts all nodes created with "add" in one go. */
//  def init() = for (n <- nodes) n.init
  def init() {
     debug("sending nodes to deployer.")
     for (n <- nodes) deployer ! Task(n) 
  }
  
  def kill =
  	system.shutdown
  
  
  def awaitTermination =
  	system.awaitTermination
   
  private def debug(msg: String) {
//    println("[DEPL] "+msg)
   }

}