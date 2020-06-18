package cosc250.roboScala

import java.awt.BorderLayout
import java.util.{Timer, TimerTask}

import javax.swing.{JFrame, JPanel}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.stream.scaladsl.{Flow, Sink, Source}

import scala.collection.mutable

object Main {


  val actorSystem: ActorSystem = ActorSystem.create("RoboScala")

  /* The game actor is created as Main is loaded */
  val gameActor: ActorRef = actorSystem.actorOf(GameActor.props)

  /* The game actor is created as Main is loaded */
  val insultsActor: ActorRef = actorSystem.actorOf(InsultsActor.props)

  /* The game actor is created as Main is loaded */
  val commandStreamActor: ActorRef = actorSystem.actorOf(CommandStreamActor.props)

  /* A timer that will send tick messages */
  val timer = new Timer

  def main(args: Array[String]): Unit = {
    /*
     * Let's create the actors for the players. They should register themselves
     */
    actorSystem.actorOf(Props(classOf[InsultingDuck]))

    actorSystem.actorOf(Props(classOf[WallPatroller]))

    actorSystem.actorOf(Props(classOf[MyVeryOwnTank]))

    actorSystem.actorOf(Props(classOf[SpinningDuck]))

  }

  /** Called by the button on the GameUI */
  def startGame(): Unit = {
    val task = new TimerTask {
      def run(): Unit = {
        gameActor ! Tick(System.currentTimeMillis())
      }
    }
    timer.schedule(task, 16L, 16L)
  }
}