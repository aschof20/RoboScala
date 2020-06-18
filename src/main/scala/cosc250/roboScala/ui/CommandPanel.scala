package cosc250.roboScala.ui

import java.awt.{Color, GridLayout}
import javax.swing.{BoxLayout, JLabel, JPanel}

import cosc250.roboScala._

/** A natty little component that shows the commands a tank is giving */
class CommandPanel(val name:String) extends JPanel {

  class CommandLabel(c:Command) {
    val label = new JLabel(c.toString)
    label.setOpaque(true)

    def update(commands:Set[Command]):Unit = {
      if (commands.contains(c)) {
        label.setBackground(Color.yellow)
        label.repaint()
      } else {
        label.setBackground(Color.gray)
        label.repaint()
      }
    }
  }

  setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS))

  val commandLabels = Seq(
    new CommandLabel(FullSpeedAhead),
    new CommandLabel(FullReverse),
    new CommandLabel(TurnAnticlockwise),
    new CommandLabel(TurnClockwise),
    new CommandLabel(TurretAnticlockwise),
    new CommandLabel(TurretClockwise),
    new CommandLabel(RadarAnticlockwise),
    new CommandLabel(RadarClockwise),
    new CommandLabel(RadarPing),
    new CommandLabel(Fire),
  )

  val gridPanel = new JPanel(new GridLayout(5, 2, 4, 4))
  commandLabels.foreach(l => gridPanel.add(l.label))

  val titleLabel = new JLabel(s"<html><h3>$name</h3></html>")
  titleLabel.setAlignmentX(0.5f)

  add(titleLabel)
  add(gridPanel)

  def update(commands:Set[Command]):Unit = {
    for { l <- commandLabels } l.update(commands)
    repaint()
  }

}
