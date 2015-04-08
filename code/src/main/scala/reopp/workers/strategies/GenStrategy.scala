package reopp.workers.strategies

import reopp.common.guardedcommands.GCSolution
import reopp.common.guardedcommands.Formula
import reopp.common.guardedcommands.GCConnector.GCBuilder
import reopp.workers.Deployer
import reopp.common.CBuilder
import reopp.workers.Engine

object GenEngine {
  private type S = GCSolution
  private type C = Formula
  

  /** Create and start a deployer that uses the OneStep strategy, for guarded commands. */
  def oneStep(workers:Int) = { 
	  new Engine[S,C,OneStepStrategy[S,C]](workers)
  }

  /** Create and start a deployer that uses the Hybrid strategy, for guarded commands. */
  def hybrid(workers:Int) = { 
	  new Engine[S,C,HybridStrategy[S,C]](workers)
  }

  /** Create and start a deployer that uses the Complete strategy, for guarded commands. */
  def all(workers:Int) = { 
	  new Engine[S,C,CompleteStrategy[S,C]](workers)
  }
}