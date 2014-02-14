package reopp.workers.strategies

import reopp.common.guardedcommands.GCSolution
import reopp.common.guardedcommands.Formula
import reopp.common.guardedcommands.GCConnector.GCBuilder
import reopp.workers.Deployer
import reopp.common.CBuilder

object GenDeployer {
  private type S = GCSolution
  private type C = Formula
  

  /** Create and start a deployer that uses the OneStep strategy, for guarded commands. */
  def oneStep(workers:Int) = { 
	  val d = new Deployer[S,C,OneStepStrategy[S,C]](workers)
	  d.start
	  d
  }

  /** Create and start a deployer that uses the Hybrid strategy, for guarded commands. */
  def hybrid(workers:Int) = { 
	  val d = new Deployer[S,C,HybridStrategy[S,C]](workers)
	  d.start
	  d
  }

  /** Create and start a deployer that uses the Complete strategy, for guarded commands. */
  def all(workers:Int) = { 
	  val d = new Deployer[S,C,CompleteStrategy[S,C]](workers)
	  d.start
	  d
  }
}