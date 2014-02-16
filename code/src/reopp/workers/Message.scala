package reopp.workers

import reopp.common.Constraints
import reopp.common.Solution
import reopp.workers.strategies.Strategy

sealed abstract class Message

// From Worker to Conflict manager (Claim also from Deployer to Worker)
case class Claim(nd:AnyRef) extends Message
case class IgnoredClaim(nd:AnyRef) extends Message
case class StratNd(str:AnyRef,nd:AnyRef) extends Message
case class Updated(nd:Iterable[AnyRef])
case object Success extends Message
case object Fail extends Message

// From Conflict manager to Worker
case class Claimed(nd:AnyRef) extends Message
case class Strat(str:AnyRef) extends Message
case object Quit extends Message
case class QuitAndUpdate(nd:Iterable[AnyRef])
case class GiveUp(nd:AnyRef) extends Message

// From Conflict manager to Deployer
case object WorkerDone extends Message
case class Task(nd:AnyRef) extends Message

// From Deployer to Conflict manager, and from the outside to Deployer.
case object Status extends Message
