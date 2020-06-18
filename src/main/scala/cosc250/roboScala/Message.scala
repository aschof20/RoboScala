package cosc250.roboScala

import akka.actor.ActorRef
import akka.stream.scaladsl.Sink

/** Messages are received by AI Players from the Game Actor */
sealed trait Message

/** A list of names of alive tanks still in the game. Sent whenever this changes. */
case class TanksAlive(names:Seq[String]) extends Message

/** The current state of your tank */
case class TankState(tank:Tank) extends Message

/** If your shell hit a tank */
case class Hit(target:Tank) extends Message

/** If your shell expired, having missed everything */
case class Missed(shell:Shell) extends Message

/** The latest results from your radar */
case class RadarResult(you:Tank, tanks:Set[Tank]) extends Message

/** A message you'll receive if you've just been insulted */
case class Insulted(insult:String) extends Message

/** A Tick is sent by the timer, to cause the Game Actor to move the game forward */
case class Tick(time:Long)

/** Sent by Main to say the game is ready to show */
case object Ready

/** Asks the GameActor to look up a tank name from an ActorRef */
case class WhoIs(ar:ActorRef)

/** Asks the GameActor to look up an ActorRef for a named tank */
case class ActorRefOf(name:String)

/** Sent by the UI to the GameActor to listen to the stream of events */
case class RegisterStreamSink[Mat](sink:Sink[(String, Command), Mat])
