package cosc250.roboScala.ui

import java.awt._
import javax.swing.JPanel

import cosc250.roboScala._

/**
  * A panel for painting our tanks. Notice that gameState is call-by-name -- it
  * will re-query the variable each time.
  */
class GamePanel(gameState: => GameState) extends JPanel {

  val bg:Color = Color.DARK_GRAY

  val wallColor:Color = Color.orange

  val turretColor:Color = Color.lightGray

  val radarConeColor = new Color(1.0f, 1.0f, 1.0f, 0.25f)

  override def getPreferredSize = new Dimension(GameState.width, GameState.height)

  override def paint(graphics:Graphics):Unit = {
    val g = graphics.asInstanceOf[Graphics2D]

    /** Wraps any code so that the graphics' transform will be reset at the end*/
    def resettingTransform(f: => Unit):Unit = {
      val oldTransform = g.getTransform
      f
      g.setTransform(oldTransform)
    }

    def drawRadarCone(t:Tank):Unit = resettingTransform {
      val Vec2(x, y) = t.position
      g.translate(x, y)
      g.rotate(t.facing + t.turretAngle + t.radarAngle)

      g.setColor(radarConeColor)
      g.fill(Tank.radarCone)
    }

    def drawTank(t:Tank, commands:Set[Command]):Unit = resettingTransform {
      val Vec2(x, y) = t.position
      val color = if (t.health <= 0) {
        Color.black
      } else if (commands.contains(TakeHit)) {
        Color.red
      } else {
        t.color
      }

      // Draw the tank
      g.translate(x, y)
      g.rotate(t.facing)
      g.setColor(color)
      g.fill(Tank.tankBody)

      // Draw the turret
      g.rotate(t.turretAngle)
      g.setColor(turretColor)
      Tank.turretShapes.foreach(g.fill)

      // Draw the radar
      g.rotate(t.radarAngle)
      g.setColor(color)
      g.setStroke(new BasicStroke(3))
      Tank.radarShapes.foreach(g.draw)
    }

    def drawShell(s:Shell):Unit = resettingTransform {
      val Vec2(x, y) = s.position
      g.translate(x, y)
      g.rotate(s.velocity.theta)

      g.setColor(Color.red)
      g.fill(Shell.shellShape)
    }

    def drawLegend(t:Tank):Unit = resettingTransform {
      val Vec2(x, y) = t.position

      g.setColor(Color.green)

      val nameW = g.getFontMetrics.stringWidth(t.name)
      g.drawString(t.name, x.toInt - nameW/2, y.toInt + 30)

      val legend = s"H:${t.health.toInt} E:${t.energy.toInt}"
      val legendW = g.getFontMetrics.stringWidth(legend)
      g.drawString(legend, x.toInt - legendW/2, y.toInt + 45)
    }

    // Clear the background
    g.setColor(bg)
    g.fillRect(0, 0, getWidth, getHeight)

    // Mark the centre of the pitch
    g.setColor(Color.gray)
    g.setStroke(new BasicStroke(1))
    val midX = GameState.width/2
    val midY = GameState.height/2
    g.drawLine(midX - 3, midY - 3, midX + 3, midY + 3)
    g.drawLine(midX - 3, midY + 3, midX + 3, midY - 3)

    // Mark the edge of the pitch
    val dashed = new BasicStroke(3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 0, Array[Float](9), 0)
    g.setStroke(dashed)
    g.drawRect(0, 0, GameState.width, GameState.height)

    // Cache the gameState in a local variable
    val gameState = this.gameState

    // Paint the tanks
    for { t <- gameState.tanks  } {
      drawTank(t, gameState.commands.getOrElse(t.name, Set.empty))
    }

    // Paint the shells
    for { s <- gameState.shells } drawShell(s)

    // Draw the radar cones for any radars that are pinging this tick
    for {
      t <- gameState.tanks if {
        t.canPing && gameState.commands.getOrElse(t.name, Set.empty).contains(RadarPing)
      }
    } drawRadarCone(t)

    // Draw the legends
    for { t <- gameState.tanks  } drawLegend(t)


  }

}
