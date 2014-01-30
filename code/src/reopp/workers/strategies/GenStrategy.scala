package reopp.workers.strategies

import reopp.common.guardedcommands.GCSolution
import reopp.common.guardedcommands.Formula
import reopp.common.guardedcommands.GCConnector.GCBuilder
import reopp.workers.Deployer
import reopp.common.CBuilder

object GenDeployer {
  private type S = GCSolution
  private type C = Formula
  

  def oneStep(workers:Int) = { 
	  new Deployer[S,C,OneStepStrategy[S,C]](workers)
  }

  def hybrid(workers:Int) = { 
	  new Deployer[S,C,HybridStrategy[S,C]](workers)
  }

  def all(workers:Int) = { 
	  new Deployer[S,C,CompleteStrategy[S,C]](workers)
  }
}