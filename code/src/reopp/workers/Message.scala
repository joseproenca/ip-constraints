package reopp.workers

import reopp.common.Constraints
import reopp.common.Solution
import reopp.workers.strategies.Strategy

sealed abstract class Message

case class Claim(nd:AnyRef) extends Message
case class Claimed(nd:AnyRef) extends Message
case class IgnoredClaim(nd:AnyRef) extends Message
case class StratNd(str:AnyRef,nd:AnyRef) extends Message
case class Strat(str:AnyRef) extends Message
case object Quit extends Message
case class QuitAndUpdate(nd:Iterable[AnyRef])
case class Updated(nd:Iterable[AnyRef])
case class GiveUp(nd:AnyRef) extends Message
case class GetBack(nd:AnyRef) extends Message
case object Success extends Message
case object Fail extends Message
case object Exit extends Message
case object WorkerDone extends Message

