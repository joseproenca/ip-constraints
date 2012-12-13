package dreams

import actors.OutputChannel
import common.{Constraints, Solution}


/**
 * Created by IntelliJ IDEA.
 * User: jose
 * Date: 02/05/12
 * Time: 13:48
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class  Message

//case object Init                  extends Message
case object Suspend               extends Message
case class RequestBeh(rank:Int)   extends Message
case class StrongerReq(rank:Int)  extends Message
case class ReplyBeh[C<:Constraints[Solution,C]](c:C) extends Message
case class ReplySol[S<:Solution](sol:S)            extends Message
case class ReplyData(d:Any)                        extends Message
case class ReplySolData[S<:Solution](sol:S,d:Any)  extends Message
case class Update[S<:Solution](sol:S)    extends Message
case object Busy                  extends Message
case class Admin(msg:Any)         extends Message


//sealed class ReconfigMsg
case class Connect(a:OutputChannel[Any]) extends Message
case object Release                      extends Message