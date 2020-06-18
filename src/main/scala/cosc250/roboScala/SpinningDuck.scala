package cosc250.roboScala

import java.awt.Color
import akka.actor.Actor
import scala.util.Random

/**
  * Spinning duck turns in circles, while also spinning its turret and radar (making its
  * radar sweep fast). It pings constantly, and if it sees a tank always fires.
  */
class SpinningDuck extends Actor {

  // Give our tank a unique name
  val name = s"Spinning Duck ${Random.alphanumeric.take(4).mkString}"

  // As soon as the tank is created, register it
  Main.gameActor ! Register(name, Color.orange)

  def receive:PartialFunction[Any, Unit] = {

    // Every game tick, we receive our tank state, and should send commands to
    // Main.gameActor to say what we want to do this tick
    case TankState(me) =>
      Main.gameActor ! FullSpeedAhead
      if (me.canPing) Main.gameActor ! RadarPing
      Main.gameActor ! TurnClockwise
      Main.gameActor ! TurretClockwise
      Main.gameActor ! RadarClockwise

    // If we successfully Ping the radar, we'll get a message containing the
    // states of any tanks we see
    case RadarResult(me, seenTanks) =>
      if (seenTanks.nonEmpty) sender() ! Fire

  }

}
