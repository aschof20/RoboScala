package cosc250.roboScala.ui

import java.awt.{BorderLayout, Color}
import java.awt.event.ActionEvent

import akka.NotUsed
import akka.actor.ActorSystem
import akka.io.Udp.Send
import javax.swing._
import akka.stream.scaladsl.{Flow, Sink, Source, SourceQueue}
import cosc250.roboScala
import cosc250.roboScala._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** The UI for a game. Note that it takes a by-name argument. */
class GameUI(gameState: => GameState, commands: => Map[String, Set[Command]]) {

  // Draws the tanks, etc
  val gamePanel = new GamePanel(gameState)

  // Starts the game
  val startButton = new JButton("Start game!")
  startButton.addActionListener((e:ActionEvent) => Main.startGame())

  /** The eastern panel, showing the log of message */
  val messagesPanel = new Box(BoxLayout.Y_AXIS)
  val messageTextArea = new JTextArea("Commands and messages from the tanks will appear here once a stream is registered")
  messageTextArea.setLineWrap(true)
  messageTextArea.setWrapStyleWord(true)
  messageTextArea.setColumns(20)
  val messages:mutable.Queue[(String, Command)] = mutable.Queue.empty
  var messageFilter: (String, Command) => Boolean = (_, _) => true

  /** Filters the message pane so that only insults are shown */
  val insultsFilterButton = new JButton("Insults")
  insultsFilterButton.addActionListener { (_) =>
    messageFilter = {
      case (_, _:InsultCommand) => true
      case _ => false
    }
    updateLog()
  }

  /** Filters the message pane so that only fires and shots are shown */
  val hitFilterButton = new JButton("Shot actions")
  hitFilterButton.addActionListener { (_) =>
    messageFilter = {
      case (_, TakeHit) => true
      case (_, Fire) => true
      case _ => false
    }
    updateLog()
  }

  val filters = new Box(BoxLayout.LINE_AXIS)
  filters.add(insultsFilterButton)
  filters.add(hitFilterButton)
  messagesPanel.add(filters)
  messagesPanel.add(messageTextArea)
  messagesPanel.add(Box.createGlue())

  register()

  /** The western panel, holding the state of all the tanks */
  private val commandsPanel = new Box(BoxLayout.Y_AXIS)
  commandsPanel.add(Box.createGlue())

  /** Each command panel shows the state of a single tank */
  private val commandPanels = mutable.Buffer.empty[CommandPanel]

  /** The main window */
  val frame = new JFrame("RoboScala")
  frame.setLayout(new BorderLayout())
  frame.add(gamePanel, BorderLayout.CENTER)
  frame.add(messagesPanel, BorderLayout.EAST)
  frame.add(commandsPanel, BorderLayout.WEST)
  frame.add(startButton, BorderLayout.SOUTH)
  frame.setSize(1124, 768)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setVisible(true)

  /** Called by the game actor to request the game panel be repainted */
  def repaint():Unit = {
    val commands = this.commands
    for { p <- commandPanels } p.update(commands.getOrElse(p.name, Set.empty))

    gamePanel.repaint()
  }

  /** Adds a tank to the ui */
  def addTank(name:String):Unit = {
    val p = new CommandPanel(name)
    commandPanels.append(p)
    commandsPanel.add(p, 0)

    // trigger a re-layout
    frame.revalidate()
  }

  /** Pushes a command from a tank into the commands that will be rendered. */
  def pushCommand(t:String, c:Command):Unit = {
    if (messageFilter(t,c)) {
      messages.enqueue((t, c))

      if (messages.lengthCompare(12) > 0) messages.dequeue()
      updateLog()
    }
  }
   /** Updates the messageTextArea to display the commands in the log */
  def updateLog():Unit = {
    SwingUtilities.invokeLater { () =>
      val buf = new mutable.StringBuilder
      for {
        (tank, command) <- messages if messageFilter(tank, command)
      } {
        command match {
          case Insult(t, insult) =>
            buf.append(s"$tank insults $t \n")
            buf.append(insult)
            buf.append("\n\n")
          case Retort(insult) =>
            buf.append(s"$tank retorts \n")
            buf.append(insult)
            buf.append("\n\n")
          case TakeHit =>
            buf.append(s"$tank got hit!\n")
          case Fire =>
            buf.append(s"$tank fires a shell...\n")
          case _ =>
            buf.append(s"$tank command \n") // do nothing
        }
      }
      messageTextArea.setText(buf.mkString)
      messageTextArea.repaint()
    }
  }

  def register():Unit = {

    val sink = Sink.foreach[(String, Command)]{

      case(tank, command) => pushCommand(tank, command )}

    Main.commandStreamActor ! RegisterStreamSink(sink)
  }
}
