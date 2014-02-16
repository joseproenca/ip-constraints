package reopp.common.examples

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.ActorSystem


class Snd extends Actor {
  def receive = {
      case 'PING => 
        println("got some ping... sending pong.")
        sender ! 'PONG
        context.stop(self)
      case _ => 
  }   
}

class Fst extends Actor {
	val child = context.actorOf(Props[Snd])
	
	
  def receive = {
		case 'GO => child ! 'PING
    case 'PONG =>
      println("got pong. sending strange thing.")
    sender ! "AAAA"
    context.system.shutdown
  }
}


object Driver extends App {

  val system = ActorSystem("System")
  val fst = system.actorOf(Props[Fst], name = "fst")
	
//	val fst = new Fst
  fst ! 'GO
//  system.shutdown
  
}