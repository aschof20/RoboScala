package cosc250.roboScala

import java.awt.Shape
import java.awt.geom.{AffineTransform, Rectangle2D}

/** A shell is fired by a tank */
case class Shell(firedBy:String, position:Vec2, velocity:Vec2) {

  /** Which direction the tanks is going */
  def facing:Double = velocity.theta

  /** Used for collision (hit) detection */
  def bounds:Shape = {
    val at = new AffineTransform()
    val Vec2(x, y) = position

    at.translate(x, y)
    at.rotate(facing)

    at.createTransformedShape(Shell.shellShape)
  }

  /** Called by the GameActor to work out where the shell moves next */
  def update(dt:Double):Shell = copy(position = position + velocity * dt)

}

object Shell {

  /** How to draw a shell. Also used in hit detection. */
  val shellShape = new Rectangle2D.Double(-2, -2, 4, 4)

  /** How much energy it costs to fire a shell */
  val energy = 30d
  
  val velocity = 80

}