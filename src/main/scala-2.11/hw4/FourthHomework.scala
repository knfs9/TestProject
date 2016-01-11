package hw4

import akka.actor._
import hw4.LoggerMonad.Util._

/*
Домашнее задание на этот раз:

  Создать приложение, удовлетворяющее следующим критериям:
      Создать монаду LoggerMonad, позволяющую логировать события.
      Акторы должны использовать LoggerMonad.
 */
trait CustomActions {

  class MyActorActions {

    case object Ping

    case object Pong

    def getAction(x: String) = x.toUpperCase() match {
      case "PONG" => Pong
      case _ => Ping
    }
  }

}

trait ActorComponent extends CustomActions {
  val actorActionsProvider = new MyActorActions
}

trait FirstActorComponentImpl extends ActorComponent {
  this: FirstActorComponentImpl with CustomActions =>

  class FirstActorImpl(startAction: String, n: Int, secondPlayer: ActorRef) extends Actor {

    val firstAction = actorActionsProvider.getAction(startAction)
    var sent = 0

    @throws[Exception](classOf[Exception])
    override def preStart(): Unit = {
      val r = ("First actor started the game from " + firstAction) ~> (secondPlayer ! firstAction)
      r.log foreach println
      sent += 1
    }

    override def receive: Receive = {
      case actorActionsProvider.Ping =>
        val r = ("First actor send " + actorActionsProvider.Pong) ~> (sender ! actorActionsProvider.Pong)
        r.log foreach println
        if (sent < n) sent += 1 else context.stop(self)
      case actorActionsProvider.Pong =>
        val r = ("First actor send " + actorActionsProvider.Ping) ~> (sender ! actorActionsProvider.Ping)
        r.log foreach println
        if (sent < n) sent += 1 else context.stop(self)
    }

  }

}

trait SecondActorComponentImpl extends ActorComponent {
  this: SecondActorComponentImpl with CustomActions =>

  class SecondActorImpl extends Actor {

    import hw4.LoggerMonad.Util._

    override def receive: Receive = {
      case actorActionsProvider.Ping =>
        val r = ("Second actor send " + actorActionsProvider.Pong) ~> (sender ! actorActionsProvider.Pong)
        r.log foreach println
      case actorActionsProvider.Pong =>
        val r = ("Second actor send " + actorActionsProvider.Ping) ~> (sender ! actorActionsProvider.Ping)
        r.log foreach println
    }

  }

}

class Terminator(ref: ActorRef) extends Actor {
  context watch ref

  override def receive: Actor.Receive = {
    case Terminated(_) =>
      context.system.terminate()
  }
}

object FourthHomework extends App with FirstActorComponentImpl with SecondActorComponentImpl {
  val system = ActorSystem("ping-pong")
  val startMessage = "Ping"
  val attempts = 5

  val playerTwo = system.actorOf(Props(new SecondActorImpl), "secondPlayer")
  val playerOne = system.actorOf(Props(new FirstActorImpl(startMessage, attempts, playerTwo)), "firstPlayer")

  system.actorOf(Props(classOf[Terminator], playerOne), "terminatorOne")
  system.actorOf(Props(classOf[Terminator], playerTwo), "terminatorTwo")
}