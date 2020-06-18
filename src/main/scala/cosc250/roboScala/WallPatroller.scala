package cosc250.roboScala

import java.awt.Color
import akka.actor.Actor
import scala.util.Random

/** WallPatroller patrols the perimeter of the playing field in a clockwise direction,
  * patrolling for enemies to destroy.
  */
class WallPatroller extends Actor {

  // Give our tank a unique name.
  val name = s"WallPatroller ${Random.alphanumeric.take(4).mkString}"

  // As soon as the tank is created, it is registered.
  Main.gameActor ! Register(name, Color.pink)

  // The origin of the playing field.
  val fieldOrigin:Vec2 = Vec2(GameState.width/2, GameState.height/2)

  /** Function to calculate the position vector one step into the future using
    * the current velocity, position and facing
    * @param me - the tank whose future position is to be analysed.
    * @param time - the time that one step.
    * @return Vec2 of the tank position one step into the future.
    */
  def futurePosition(me: Tank, time: Int): Vec2 = {
    me.position + Vec2.fromRTheta(me.velocity * time, me.facing)
  }

  /** A function the calls turretSteering(), which calculates and returns
    * the angle of the turretFacing to point towards a given target. This
    * angle is then used to determine the direction of the turrets travel.
    * @param me     - the tank whose turret is to be move to point a target.
    * @param target - the target the tanks turret is to point to.
    */
  def turretHeading(me:Tank, target: Vec2):Unit ={

    /* Function that calculates the turretFacing angle so that the turret faces
     * towards a specified target.
     */
    def turretSteering(me: Tank, target: Vec2):Double = {
      val vectorDelta = me.position - target
      val angleDelta = Math.atan(vectorDelta.y/vectorDelta.x)

      //Calculate the turret facing depending on the location within the quadrants.
      if(vectorDelta.x < 0 && vectorDelta.y < 0) me.turretFacing - angleDelta
      else if(vectorDelta.x < 0 && vectorDelta.y > 0) me.turretFacing - (angleDelta + 2*Math.PI)
      else if(vectorDelta.x >  0 && vectorDelta.y > 0) me.turretFacing - (angleDelta + Math.PI)
      else if(vectorDelta.x >  0 && vectorDelta.y <  0)me.turretFacing - (angleDelta  + Math.PI)
      else me.turretFacing
    }

    /* Determine the movement of the turret based on the turretFacing required to point
    to the target.*/
    if (turretSteering(me, target) > 0 && turretSteering(me, target) < Math.PI)
      Main.gameActor ! TurretAnticlockwise
    else
      Main.gameActor ! TurretClockwise
  }

  /** Function that determines if a tank is within the field.
    * If the tank is in the field it continues with constant forward
    * motion, otherwise it turns clockwise.
    * @param me - tank whose heading is being calculated.
    */
  def tankHeading(me: Tank): Unit = {
    if (GameState.inBounds(futurePosition(me , 1)))
      Main.gameActor ! FullSpeedAhead
    else
      Main.gameActor ! TurnClockwise
  }

  /** Function that determines if a tank has sufficient energy, if it does
    * the radar will ping
    * @param me - tank whose energy is tested.
    */
  def radarPing(me: Tank):Unit ={
    if(me.energy == Tank.startingEnergy) Main.gameActor ! RadarPing
  }

  /** Function the determines if the 'me' tank has seen another live tank and
    * commences firing at them if 'me' has sufficient energy.
    * @param me - tank that is surveying field for other tanks.
    * @param seenTanks - set of tanks seen by 'me' tank.
    */
  def commenceFiring(me:Tank, seenTanks: Set[Tank]): Set[Unit] ={
    for {tank <- seenTanks} yield {
      if (tank.isAlive && me.canFire) sender() ! Fire
    }
  }

  def receive:PartialFunction[Any, Unit] = {

    case TankState(me) =>
      /** Call the tankHeading function. While the future position is within the playing field
        * the tank drive ahead. If the future position is out of bounds, the tank will turn
        * clockwise to round the corner and keep the tank inbounds. */
      tankHeading(me)

      /** Call the turretHeading function. If the angle to the center of the field
        * is greater than 0 and less than PI (i.e. in Quadrants I or II) then the
        * turret turns anticlockwise. Otherwise it turns clockwise to continually
        * point to the center of the field. */
      turretHeading(me, fieldOrigin)

      /** Call the radarPing function. If the tank has energy equal to its
        * startling energy then it will ping the radar when encountering
        * other tanks on the field*/
      radarPing(me)

    case RadarResult(me, seenTanks) =>
      /** Iterate over the set of seenTanks. If the seenTank energy if greater than 0
        * (i.e they are alive) and 'me' is capable of firing, fire shells at seenTank. */
      commenceFiring(me, seenTanks)
  }
}