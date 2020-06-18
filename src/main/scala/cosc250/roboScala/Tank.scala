package cosc250.roboScala

import java.awt.{Color, Polygon, Shape}
import java.awt.geom.{AffineTransform, Arc2D, Ellipse2D, Rectangle2D}


/**
  * The current state of a tank
  *
  * @param name the name should be unique (no two tanks on the field with the same name)
  * @param position where it is
  * @param facing the angle in radians it is facing
  * @param velocity its current velocity
  * @param health when zero, the tank dies
  * @param energy energy recharges over time and is needed to ping the radar and fire shells
  * @param turretAngle the angle of the turret with respect to the tank body
  * @param radarAngle the angle of the radar with respect to the turret
  * @param color what colour is my tank
  */
case class Tank(name:String,
                position:Vec2,
                facing:Double, velocity:Double = 0,
                health:Double, energy:Double,
                turretAngle:Double = 0,
                radarAngle:Double = 0, radarPowered:Boolean = false,
                color:Color = new Color(
                  Math.random().toFloat,
                  Math.random().toFloat,
                  Math.random().toFloat
                )
               ) {

  /** Calculates the angle with respect to the world that the turret is pointing in */
  def turretFacing:Double = (facing + turretAngle) % (Math.PI * 2)

  /** Calculates the angle with respect to the world that the radar is pointing in */
  def radarFacing:Double = (facing + turretAngle + radarAngle) % (Math.PI * 2)

  /** Whether this tank is alive */
  def isAlive:Boolean = health > 0

  /** Whether this tank has enough energy to fire */
  def canFire:Boolean = isAlive && energy > Shell.energy

  /** Whether this tank has enough energy to ping the radar */
  def canPing:Boolean = isAlive && energy > Tank.radarPower

  // TODO: use for
  /** Called by the GameActor to work out what happens to the tank. */
  def update(dt:Double, commands:Set[Command]):Tank = {

    val nextPos = position + Vec2.fromRTheta(velocity, facing) * dt
    val nextVelocity = velocity * 0.9

    val stateBeforeCommands = this.copy(
      position = nextPos,
      velocity = nextVelocity,
      energy = Math.min(this.energy + Tank.rechargePower, Tank.startingEnergy)
    )

    commands.foldLeft(stateBeforeCommands) {

      case (tank, FullSpeedAhead) =>
        val newVel = Math.min(velocity + Tank.engineForce * dt, Tank.maxSpeed)
        tank.copy(velocity = newVel)

      case (tank, FullReverse) =>
        val newVel = Math.max(velocity - Tank.engineForce * dt, -Tank.maxSpeed)
        tank.copy(velocity = newVel)

      case (tank, TurnClockwise) =>
        tank.copy(facing = (tank.facing + Tank.turnForce * dt) % (Math.PI * 2))

      case (tank, TurnAnticlockwise) =>
        tank.copy(facing = (tank.facing - Tank.turnForce * dt) % (Math.PI * 2))

      case (tank, TurretClockwise) =>
        tank.copy(turretAngle = (tank.turretAngle + Tank.turredForce * dt) % (Math.PI * 2))

      case (tank, TurretAnticlockwise) =>
        tank.copy(turretAngle = (tank.turretAngle - Tank.turredForce * dt) % (Math.PI * 2))

      case (tank, RadarClockwise) =>
        tank.copy(radarAngle = (tank.radarAngle + Tank.radarForce * dt) % (Math.PI * 2))

      case (tank, RadarAnticlockwise) =>
        tank.copy(radarAngle = (tank.radarAngle - Tank.radarForce * dt) % (Math.PI * 2))

      case (tank, RadarPing) =>
        tank.copy(energy = Math.max(0, tank.energy - Tank.radarPower))

      case (tank, Fire) =>
        tank.copy(energy = Math.max(0, tank.energy - Shell.energy))

      case (tank, TakeHit) =>
        tank.copy(health = Math.max(0, tank.health - Shell.energy))

      case (tank, _) => tank
    }

  }

  /** Used for hit detection */
  def bodyIntersects(r:Rectangle2D):Boolean = {
    val at = new AffineTransform()
    val Vec2(x, y) = position

    at.translate(x, y)
    at.rotate(facing)

    at.createTransformedShape(Tank.tankBody).intersects(r)
  }

  /** Used for radar detection */
  def transformedBody:Shape = {
    val at = new AffineTransform()
    val Vec2(x, y) = position

    at.translate(x, y)
    at.rotate(radarFacing)

    at.createTransformedShape(Tank.tankBody)
  }

  /** Used for radar detection -- the shape on the field of the area scanned by the radar */
  def transformedRadarCone:Shape = {
    val at = new AffineTransform()
    val Vec2(x, y) = position

    at.translate(x, y)
    at.rotate(radarFacing)

    at.createTransformedShape(Tank.radarCone)
  }

  /** Hit detection with shells */
  def hitBy(shell:Shell):Boolean = {
    bodyIntersects(shell.bounds.getBounds2D)
  }

}


object Tank {

  val maxSpeed = 40

  val engineForce = 80d  // pixels / s^2

  val turnForce = 4d

  val turredForce = 1d // radians / s

  val radarForce = 3d // radians / s

  val startingHealth = 100d

  val startingEnergy = 100d

  val rechargePower = 0.5d

  val radarPower = 0.5d

  val standardBulletStrength = 20d

  /** Affects where shells are fired from */
  val barrelLength = 30

  /** The shape of a turret, drawn on the screen */
  val turretShapes:Seq[Shape] = Seq(
    new Ellipse2D.Double(-12, -12, 24, 24),
    new Rectangle2D.Double(0, -2, barrelLength, 4)
  )

  /** The body is the part of the tank that can be hit */
  val tankBody:Shape = new java.awt.Polygon(
    Array(-20, 20, 20, -20),
    Array(-20, -15, 15, 20),
    4
  )

  /** The radar cone is the shape in which the radar can detect other tanks */
  val radarCone:Shape = new Polygon(
    Array(0, 250, 250, 0),
    Array(-4, -100, 100, 4),
    4
  )

  /** The radar is drawn to indicate which way it is facing */
  val radarShapes:Seq[Shape] = Seq(
    new Arc2D.Double(0, -5, 10, 10, 125, 110, Arc2D.OPEN)
  )

  /** Create a new tank in a random location. */
  def random(name:String): Tank = Tank(
        name = name,
        position=Vec2(20 + Math.random() * 600, 20 + Math.random() * 440),
        facing = Math.random() * Math.PI * 2,
        health = 100, energy = 100
      )
}