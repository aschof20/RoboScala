package cosc250.roboScala

import javax.swing._

import akka.actor.{Actor, ActorPath, ActorRef, Props}
import akka.event.Logging

import scala.collection.mutable

object GameActor {
  /** A Props object that will cause a GameActor to be created */
  val props = Props.apply(classOf[GameActor])
}

/**
  * The GameActor handles the game state. Every 16ms, it receives a tick message
  * and executes the actions on the tanks (and sends out their new states). In the
  * meantime, it waits for commands from the tanks to apply on the next tick.
  */
class  GameActor extends Actor {

  val log = Logging(context.system, this)

  /** Keeps the commands for each tank that are to be applied on the next tick. */
  private val commands = mutable.Map.empty[String, Set[Command]]

  /** Keeps the ActorRefs of the players that have registered and are taking part. */
  private val players = mutable.Map.empty[String, ActorRef]

  /** A reverse-lookup, so you can get a tank's name from who sent the command. */
  private val reversePlayersMap = mutable.Map.empty[ActorPath, String]

  /** Looks up a player. The actor might be a temporary actor, in which case we may need to look up their parent. */
  def reversePlayers(ar:ActorRef, msg:Any): Option[String] = {
    log.debug("Looking up {} on behalf of {}", ar, sender())

    var cursor = ar.path
    while (!reversePlayersMap.contains(cursor) && cursor != cursor.root) cursor = cursor.parent

    if (reversePlayersMap.contains(cursor)) {
      Some(reversePlayersMap(cursor))
    } else {
      log.error("No player found for actor {} in processing message {}", ar, msg)
      None
    }
  }

  /** The current state of the game. Mutable state updated by the actor each tick. */
  private var gameState = GameState.empty

  /** The UI for the game on the screen. Note that it takes a by-name argument. */
  val gameUi = new ui.GameUI(gameState, commands.toMap)

  /** The time of the last step we processed */
  var lastTime = System.currentTimeMillis()

  /** The last message sent about which tanks are alive */
  var lastAliveMsg = TanksAlive(Seq.empty)

  /**
    * Performs the actions needed to tick the game on.
    * You shouldn't need to edit this.
    */
  private def gameStep(dt:Double) = {

    // Handle hits. Each hit tank has a TakeHit command added, and we remember the shells
    // in order to remove them from the field (later)
    val hitShells = for {
      s <- gameState.shells
      t <- gameState.tanks if t.hitBy(s)
    } yield {
      // To register that a tank has been hit, we send a TakeHit command to the game actor (self) from the tank's actor
      self.tell(TakeHit, players(t.name))

      // Tell the player that fired the shot they hit
      players(s.firedBy) ! Hit(t)

      s
    }

    // Handle misses. For each shell that has gone out of bounds, we notify the player
    // who fired it. We remember these shells in order to remove them from the field (later)
    val missedShells = for {
      s <- gameState.shells if !GameState.inBounds(s.position)
    } yield {
      // Tell the player that fired the shot they missed
      players(s.firedBy) ! Missed(s)

      s
    }

    // Now we've added the TakeHit command where necessary, we apply commands to tanks
    // to update their state.
    val tanks = for {
      t <- gameState.tanks
    } yield {
      // Only alive tanks are updated. Dead tanks remain unchanged (but are not filtered out)
      if (t.isAlive) {
        t.update(dt, commands.getOrElse(t.name, Set.empty))
      } else t
    }

    // Perform radar detection for any tanks that have a RadarPing command and the
    // energy to make it succeed.
    for {
      t <- gameState.tanks if t.canPing && commands.getOrElse(t.name, Set.empty).contains(RadarPing)
    } {
      val cone = t.transformedRadarCone
      val seen = gameState.tanks.filter(ot => ot != t &&
        cone.intersects(ot.transformedBody.getBounds)
      )

      players(t.name) ! RadarResult(t, seen.toSet)
    }

    // Create new shells for tanks that have a Fire command and the energy to make it
    // succeed.
    val newShells:Seq[Shell] = for {
      t <- gameState.tanks if {
        t.canFire && commands.getOrElse(t.name, Set.empty).contains(Fire)
      }
    } yield {
      Shell(
        firedBy = t.name,
        position = t.position + Vec2.fromRTheta(Tank.barrelLength, t.turretFacing),
        velocity = Vec2.fromRTheta(Shell.velocity, t.turretFacing)
      )
    }

    // Work out what shells are in the next frame and where. This includes removing the
    // shells that have hit or gone out of bounds, and adding the new shells that have
    // been fired.
    val nextShells = (for {
      s <- gameState.shells diff (hitShells ++ missedShells)
    } yield s.update(dt)) ++ newShells

    // We now have enough to create the new gameState
    gameState = GameState(tanks, nextShells, commands.toMap)

    // Ask the UI to repaint
    gameUi.repaint()

    // Check whether the list of alive tanks has changed. If so, notify everyone
    val liveNames = gameState.tanks.filter(_.isAlive).map(_.name).sortBy(identity)
    if (liveNames != lastAliveMsg.names) {
      lastAliveMsg = TanksAlive(liveNames)
      for { t <- gameState.tanks } players(t.name) ! TanksAlive(liveNames)
    }

    // Clear the commands, ready to receive commands for the next game tick.
    commands.clear()

    // Tell the players what their state is. This should prompt tanks to respond with
    // their next commands.
    for { t <- gameState.tanks } players(t.name) ! TankState(t)

  }




  val receive:PartialFunction[Any, Unit] = {

    case WhoIs(ar) =>
      // Returns an Option[String] containing the tank name if there is one
      val s = sender()
      s ! reversePlayers(ar, WhoIs(ar))

    case ActorRefOf(name) =>
      val s = sender()
      s ! players(name)

    case Tick(time) =>
      val dt = (time - lastTime) / 1000.0
      lastTime = time
      gameStep(dt)

    case Register(name, color) =>
      players(name) = sender()
      reversePlayersMap(sender().path) = name
      gameState = gameState.copy(tanks = gameState.tanks :+ Tank.random(name))
      gameUi.addTank(name)
      gameUi.repaint()

    case ic:InsultCommand =>
      // Should have gone to the InsultsActor
      log.error("An insult from {} should have gone to the InsultsActor", reversePlayers(sender(), ic))

    case c:Command =>
      // Other commands are published to any subscribed streams, and then added to the tank's command set for this tick
      for {
        tank <- reversePlayers(sender(), c)
      } {
        commands(tank) = commands.getOrElse(tank, Set.empty) + c
        // Send it to the command stream actor to appear in the UI message stream
        Main.commandStreamActor ! (tank, c)
      }

    case _ => //
  }

}

