package cosc250.roboScala

import java.awt.Color
import akka.actor.Actor
import akka.event.{Logging, LoggingAdapter}
import akka.pattern.ask
import akka.util.Timeout
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}


class MyVeryOwnTank extends Actor {
  import context.dispatcher
  implicit val timeout: Timeout = 1.seconds

  // Give our tank a unique name
  val name = s"MyVeryOwnTank ${Random.alphanumeric.take(4).mkString}"
  var position: Vec2 = Vec2(GameState.width/2, GameState.height/2)

  // Empty Sequence for the seenTanks.
  var enemies: Seq[Tank] = Seq.empty

  // Set up logging.
  val log: LoggingAdapter = Logging(context.system, this)

  // Empty set used to create a set of unique insults.
  val insultSet: mutable.Set[String] = scala.collection.mutable.Set[String]()


  /** Function to calculate the position vector one step into the future using
    * the current velocity, position and facing
    * @param me   - the tank whose future position is to be analysed.
    * @param time - the time that one step.
    * @return     - Vec2 of the tank position one step into the future.
    */
  def futurePosition(me: Tank, time: Int): Vec2 = {
    me.position + Vec2.fromRTheta(me.velocity * time, me.facing)
  }

  /**
    * Function that determines the correct direction for MyVeryOwnTank to move
    * toward the enemy tank.
    * @param me         - MyVeryOwnTank in attacking mode.
    * @param enemyTank  - enemy 'me' is tracking.
    */
  def tankHeading(me: Tank, enemyTank: Vec2): Any = {

    // Helper function that calculates the angle difference between two angles.
    def angleDelta(me: Double, enemyTank: Double): Double = {
      val angle = (enemyTank - me) % (2 * Math.PI)
      if (angle <=  -1 * Math.PI)
        angle + (2 * Math.PI)
      else if (angle > Math.PI)
        angle - (2 * Math.PI)
      else angle
    }

    // Function that calculates the steering of MyVeryOwnTank to an enemy.
    def tankSteering: Double = {
      val attackAngle = (me.position - enemyTank).theta
      angleDelta(me.facing, attackAngle)
    }

    // Determine MyVeryOwnTank movement based on the steering.
    if (tankSteering > 0)
      TurnAnticlockwise
    else if (tankSteering < 0)
      TurnClockwise
  }

  /**
    * Function that determines the correct direction for MyVeryOwnTank's radar
    * to move to point at the enemy tank.
    * @param me         - MyVeryOwnTank in attacking mode.
    * @param enemyTank  - enemy 'me' is tracking.
    */
  def radarHeading(me:Tank, enemyTank: Vec2):Unit ={

    /* Function that calculates the turretFacing angle so that the turret faces
     * towards a specified target. */
    def radarSteering(me: Tank, enemyTank: Vec2):Double = {
      val vectorDelta = me.position - enemyTank
      val angleDelta = Math.atan(vectorDelta.y/vectorDelta.x)

      //Calculate the turret facing depending on the location within the quadrants.
      if(vectorDelta.x < 0 && vectorDelta.y < 0) me.radarFacing - angleDelta
      else if(vectorDelta.x < 0 && vectorDelta.y > 0) me.radarFacing - angleDelta
      else if(vectorDelta.x >  0 && vectorDelta.y > 0) me.radarFacing - (angleDelta + Math.PI)
      else if(vectorDelta.x >  0 && vectorDelta.y <  0)me.radarFacing - (angleDelta  + Math.PI)
      else me.radarFacing
    }

    /* Determine the movement of the turret based on the turretFacing required to point
    to the target.*/
    if (radarSteering(me, enemyTank) > 0 && radarSteering(me, enemyTank) < Math.PI)
      Main.gameActor ! RadarAnticlockwise
    else
      Main.gameActor ! RadarClockwise
  }

  /**
    * Function that determines the correct direction for MyVeryOwnTank's turret
    * to move to point at the enemy tank.
    * @param me         - MyVeryOwnTank in attacking mode.
    * @param enemyTank  - enemy 'me' is tracking.
    */
  def turretHeading(me:Tank, enemyTank: Vec2):Unit ={

    /* Function that calculates the turretFacing angle so that the turret faces
     * towards a specified target. */
    def turretSteering(me: Tank, enemyTank: Vec2):Double = {
      val vectorDelta = me.position - enemyTank
      val angleDelta = Math.atan(vectorDelta.y/vectorDelta.x)

      //Calculate the turret facing depending on the location within the quadrants.
      if(vectorDelta.x < 0 && vectorDelta.y < 0) me.turretFacing - angleDelta
      else if(vectorDelta.x < 0 && vectorDelta.y > 0) me.turretFacing - angleDelta
      else if(vectorDelta.x >  0 && vectorDelta.y > 0) me.turretFacing - (angleDelta + Math.PI)
      else if(vectorDelta.x >  0 && vectorDelta.y <  0)me.turretFacing - (angleDelta  + Math.PI)
      else me.turretFacing
    }

    /* Determine the movement of the turret based on the turretFacing required to point
    to the target.*/
    if (turretSteering(me, enemyTank) > 0  && turretSteering(me, enemyTank) < Math.PI)
      Main.gameActor ! TurretAnticlockwise
    else //if(turretSteering(me, target) < 0)
      Main.gameActor ! TurretClockwise
  }

  /**
    * Function that tests if the attacking tank is outside a certain radius around the
    * enemy tank and inside a buffer zone. This creates an 'attacking zone' so
    * the attacking tank can stay at a perimeter around the enemy and continue to attack
    * until the enemy is killed or it latches onto another enemy.
    * @param me        - MyVeryOwnTank in attack mode
    * @param enemyTank - the position vector of an enemy.
    * @param buffer    - buffer to be applied to the enemy.
    * @return          - true if within the attacking zone
    *                  - false if too close or far away from enemy.
    */
  def holdingZone(me: Tank, enemyTank: Vec2, buffer: Int = 75): Boolean = {
    if (me.position.x < enemyTank.x + buffer &&
      me.position.x > enemyTank.x - buffer &&
      me.position.y < enemyTank.y + buffer &&
      me.position.y > enemyTank.y - buffer) true
    else false
  }

  /** Function that calculates the distance delta between two tanks.
    * @param tank - two different tanks
    * @param  enemyTank- two different tanks
    * @return length of line between two tanks
    */
  def tankDelta(tank: Tank, enemyTank: Tank): Double = (enemyTank.position - tank.position).magnitude

  // As soon as the tank is created, register it.
  Main.gameActor ! Register(name, Color.yellow)

  /**
    * Patrolling:
    *
    * During the patrolling state the tank travels anticlockwise around the perimeter of the
    * playing field with its radar and turret constantly sweeping in a clockwise direction
    * looking for enemy tanks.
    *
    * The radar moving clockwise enables the entire field to be surveyed for enemies. The turret
    * is facing forward to reduce the movement to the enemy.
    *
    * @param patroller -  tank that is patrolling.
    * @param enemies   -  enemies the patrolling tank encounters.
    * @param time      -  time constant for future positions.
    */
  def Patrolling(patroller: Tank, enemies: Seq[Tank], time: Int = 2): Unit = {

    // If the tank is within the playing field two ticks in the future, constant movement.
    if (GameState.inBounds(futurePosition(patroller, time))) Main.gameActor ! FullSpeedAhead
    // Otherwise anticlockwise movement.
    else Main.gameActor ! TurnAnticlockwise

    // The turret in the patrolling state faces forward.
    if(patroller.turretAngle > 0 ) Main.gameActor ! TurretAnticlockwise
    else if(patroller.turretAngle < 0) Main.gameActor ! TurretClockwise

    // Radar constantly moving clockwise searching for enemies.
    Main.gameActor ! RadarClockwise
  }

  /**
    * Function that defines the Attacking state of the tank.
    *
    * Once the radar is pinged, the patroller tank switches from patrol state to attacking state.
    * When in attacking state, it moves towards the enemy and positions its radar and
    * turret to point at the enemy and fires shells. MyVeryOwnTank will continue to shoot
    * until the enemies health is 0 or there is a closer enemy.
    *
    * Considerations:
    *
    * 1.Tanks are added to a set when they are detected and continue to ping the radar.
    *   Because a set is a collection of distinct objects with no inherent order, if further
    *   tanks are detected, MyVeryOwnTank may have a case of homicidal ADD because it cannot
    *   keep track of the current target. Therefore, each tank that is detected is added to a
    *   new sequence and the sequence is ordered (ascending) by the distance between MyVeryOwnTank
    *   and the enemy (i.e. closest is the head of the seq). This way MyVeryOwnTank always attacks
    *   enemies that are closest.
    *
    * 2. To attack both static/relatively static tanks (SpinningDuck) and moving targets
    *   (Wallpatroller) with similar efficiency, the attack mode has been designed to handle
    *   each enemy differently.
    *
    *   1. Moving target: for the Wallpatroller enemy, MyVeryOwnTank points its radar and turret to
    *   the position of the Wallpatroler two time ticks into the future. This enables a more efficient
    *   hit rate.
    *
    *   2. Static target: Because the enemy stays in the same vicinity, it is not necessary to aim for
    *   the future position, therefore the current position is used when aiming the turrent and radar of
    *   MyVeryOwnTank.
    *
    * @param patroller - MyVeryOwnTank, which is attacking enemy tanks.
    * @param enemies - target of the MyVeryOwnTank.
    */
  def Attacking(patroller: Tank, enemies: Seq[Tank], time: Int = 2 ): Unit = {
    // If enemies have been detected and they are in the enemies sequence.
    if (enemies.nonEmpty) {

      // If the enemy that is the least distance from MyVeryOwnTank is not a moving tank.
      if(!enemies.head.name.startsWith("WallPatroller ")) {

        /* If MyVeryOwnTank gets too close to the enemy or it is out of the playing field it reverses.
         * Otherwise the it moves ahead. */
        if (holdingZone(patroller, enemies.head.position) && GameState.inBounds(futurePosition(patroller, time)))
          Main.gameActor ! FullReverse
        else if (GameState.inBounds(futurePosition(patroller, time))) Main.gameActor ! FullSpeedAhead

        // MyVeryOwnTank moves to the current position of the enemy.
        Main.gameActor ! tankHeading(patroller, enemies.head.position)

        // MyVeryOwnTank's turret is pointed at the current position of the enemy.
        Main.gameActor ! turretHeading(patroller, enemies.head.position)

        //MyVeryOwnTank's radar is pointed at the current position of the enemy.
        Main.gameActor ! radarHeading(patroller, enemies.head.position)

      }else{  // If the enemy that is the least distance from MyVeryOwnTank is a moving tank.

        /* If MyVeryOwnTank gets too close to the enemy or it is out of the playing field it reverses.
         * Otherwise the it moves ahead. */
        if (holdingZone(patroller, enemies.head.position) && GameState.inBounds(futurePosition(patroller, time)))
          Main.gameActor ! FullReverse
        else if (GameState.inBounds(futurePosition(patroller, time))) Main.gameActor ! FullSpeedAhead

        // MyVeryOwnTank moves to the current position of the enemy.
        Main.gameActor ! tankHeading(patroller, enemies.head.position)

        // MyVeryOwnTank's turret is pointed at the position of the enemy in two time ticks.
        Main.gameActor ! turretHeading(patroller, futurePosition(enemies.head, time))

        //MyVeryOwnTank's radar is pointed at the position of the enemy in two time ticks.
        Main.gameActor ! radarHeading(patroller,futurePosition(enemies.head, time))
      }
    }
  }

  def receive: PartialFunction[Any, Unit] = {

    case TankState(me) =>
      /* In both patrolling and attacking states, if the tank energy is 100% then the radar pings. */
      if (me.energy == Tank.startingEnergy) Main.gameActor ! RadarPing

      // If there are no enemy targets, MyVeryOwnTank is patrolling the perimeter of the field.
      if(enemies.isEmpty){
        //Call the patrolling function which implements the patrolling state.
        Patrolling(me, enemies)
      }else{ // If there are enemies that have been detected.
        //Call the Attacking function which implements the attacking state.
        Attacking(me, enemies)
      }

    /* If we successfully Ping the radar, we'll get a message containing the
      states of any tanks we see. */
    case RadarResult(me, seenTanks) =>

      /* If live enemy tanks are detected, they are added to a sequence which is ordered by their
      distance to MyVeryOwnTank. */
      if (seenTanks.nonEmpty) {

        // Filter enemy seq for live tanks and order by distance away from MyVeryOwnTank.
        enemies = seenTanks.toSeq.filter(seen => seen.isAlive).sortBy(seen => tankDelta(me, seen))

        // If enemies have been detected, MyVeryOwnTank fires at them if they are within 100 pixels.
        if (enemies.nonEmpty && tankDelta(me, enemies.head) < 100) sender() ! Fire
      }

    case Insulted(insult:String) =>

      //Sender of the insult.
      val insulter = sender()

      // Log the insult that was received.
      log.info("MyVeryOwnTank received the insult {} from {} ", insult, insulter)

      //Ask the InsultActor what the correct retort to the insult is.
      val retortToInsult = Main.insultsActor ? WhatsTheRetortFor(insult)

      // When the retort is received send it to the insulter and log the retort.
      retortToInsult.onComplete {
        case Success(retort) =>
          insulter ! retort
          log.info("{}, MyVeryOwnTank's responce is {}", insulter, retort.toString.drop(7).dropRight(1))
        case Failure(exception) => log.info("Exception {} ", exception)
      }

      /* Create a set of unique insults. */
      // Add new unique insults to the set.
      insultSet += insult

      // Log the set of insults.
      log.info("Set of insults: ({}) ", insultSet)
  }
}