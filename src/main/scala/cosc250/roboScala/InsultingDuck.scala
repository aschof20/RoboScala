package cosc250.roboScala

import java.awt.Color

import akka.actor.Actor

import scala.util.Random

/**
  * Insulting Duck just plays the insults game
  */
class InsultingDuck extends Actor {

  /** Create a unique name for this player */
  val name = s"Insulting Duck ${Random.alphanumeric.take(4).mkString}"

  /** Register the player with the game actor */
  Main.gameActor ! Register(name, Color.orange)

  /** Get the list of known insults */
  val insults: Seq[String] = InsultsActor.insults

  // The names of the other tanks that are currently alive
  var otherTanks:Seq[String] = Seq.empty

  def receive: Receive = {

    case TanksAlive(tanks) =>
      // Whenever the set of alive tanks changes (eg, start of game or a tank dies) we get a message with the names
      // of all the remaining tanks. Remove ourselves, so we don't insult ourself.
      otherTanks = tanks.filter(_ != name)

    case _ =>
      // Randomly insult a random tank
      if (otherTanks.nonEmpty && Random.nextFloat() < (1.0/60) ) {
        Main.insultsActor ! Insult(otherTanks(Random.nextInt(otherTanks.size)), insults(Random.nextInt(insults.size)))
      }
  }

}
