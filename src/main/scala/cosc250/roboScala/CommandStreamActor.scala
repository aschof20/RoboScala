package cosc250.roboScala

import akka.actor.{Actor, Props}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Sink, Source, SourceQueue}

import scala.collection.mutable
import akka.event.Logging
import akka.util.Timeout

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._

object CommandStreamActor {
  def props = Props(classOf[CommandStreamActor])
}

/**
  * Receives all the commands from the game, and allows UIs to register reactive stream sinks to receive them
  */
class CommandStreamActor extends Actor {

  import context.dispatcher
  implicit val timeout:Timeout = 1.seconds

  val log = Logging(context.system, this)

  /** Holds the queues that commands should also be published to -- see assignment. */
  private val streams = mutable.Queue.empty[SourceQueue[(String, Command)]]

  /** An implicit materialiser for when you want to run any Flows */
  implicit val materialiser = ActorMaterializer()

  /** Given a Sink, creates a source queue such that it can publish to that sink by pushing to the queue */
  def registerStream[Mat](sink:Sink[(String, Command), Mat]) = {
    val source = Source.queue[(String, Command)](10, OverflowStrategy.backpressure)
    val res = source.to(sink).run()
    streams.enqueue(res)
  }

  def receive = {

    case RegisterStreamSink(sink) =>
      // Register a new sink
      log.info("Registered a stream from {}", sender().path)
      registerStream(sink)


    case c:Command =>
      // If we receive a lone command, the sender is the tank. Look them up by asking the GameActor
      // Then push the command to all the queues (that sends them out on the streams)
      for {
        optionTank <- (Main.gameActor ? WhoIs(sender())).mapTo[Option[String]]
        tank <- optionTank
        q <- streams
      } {
        q.offer(tank, c)
      }

    case (tank:String, c:Command) =>
      // We also support receiving a tuple containing a tank name and a command
      for { q <- streams } q.offer(tank, c)

  }



}
