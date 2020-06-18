package cosc250.roboScala

import java.awt.{Color, Polygon, Shape}
import java.awt.geom.{AffineTransform, Arc2D, Ellipse2D, Rectangle2D}

case class GameState(tanks:Seq[Tank], shells:Seq[Shell], commands:Map[String, Set[Command]] = Map.empty)

object GameState {
  def empty = GameState(Seq.empty, Seq.empty)

  val width = 640

  val height = 480

  /** Returns whether a point is within the game area */
  def inBounds(p:Vec2, buffer:Double = 20):Boolean = {
    p.x > buffer && p.x < width - buffer &&
      p.y > buffer && p.y < height - buffer
  }

}