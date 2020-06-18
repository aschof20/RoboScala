package cosc250.roboScala

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._


object InsultsActor {
  val props = Props.apply(classOf[InsultsActor])

  /** The canonical list of insults and retorts */
  private val insultsAndRetorts = Seq(
    "I've seen sharper looking radar dishes in the spoons drawer" ->
      "I wondered where you get your equipment!",

    "I hear your AI whines like a baby" ->
      "No, that's the shells: a foot long, nine pounds, and screaming right at you",

    "Your aim's wobbling like you've just come out of the jelly factory" ->
      "And your chassis's moving like you've just eaten one",

    "I have suffered your insolence long enough!" ->
      "I'll try to put you out of your misery quickly, then",

    "You have the manners of a troll!" ->
      "How cute -- you think I'm family",

    "You couldn't hit the side of a bus!" ->
      "Thank goodness you don't move as fast!",

    "I can hear the gears grinding in your body" ->
      "And I can hear the gears grinding in your head",

    "Soon, you'll be sitting in a pile of smoking rubble" ->
      "It looks like you already are",

    "I've never seen a tank as incompetent as you" ->
      "You mean they always catch you unawares?",

    "I've seen dogs that were better pilots than you" ->
      "Grab the fire extinguisher -- that's not a dog you're about to hear going 'woof'",

    "You'll never see me fight as badly as you" ->
      "The smoke pouring from your engine does get in the way",

    "You'll be hearing the bang when I press this button" ->
      "A pity that's the accelerator",

    "Soon I'll be dancing on your grave!" ->
      "First you'd better dance around these shells",

    "I've half a mind to crush you like a bug!" ->
      "You mean your programmer didn't finish writing your routines?",

    "Watch me fly like the wind!" ->
      "I think it's your wind that's attracting those flies",

    "You are lower than a dung beetle!" ->
      "And you are higher than his food!",

    "My socks have more brains than you!" ->
      "Now you know where your coder went wrong!",

    "Now, vile snake, hear me roar!" ->
      "You're that angry with your coder?"

  )

  private def retortsMap = insultsAndRetorts.toMap

  def insults = insultsAndRetorts.map(_._1)

}

/**
  * The insults actor handles the "tankfighting with insults" part of the game.
  */
class InsultsActor extends Actor {

  import context.dispatcher
  implicit val timeout:Timeout = 1.seconds

  val log = Logging(context.system, this)

  def receive = {
    case WhatsTheRetortFor(i) =>
       sender() ! Retort(InsultsActor.retortsMap.getOrElse(i, "I am rubber, you are glue!"))

    case Insult(tank, insult) =>
      val s = sender()
      log.info("{} is being insulted by {}", tank, sender())

      Main.commandStreamActor forward Insult(tank, insult)

      for {
        dest <- (Main.gameActor ? ActorRefOf(tank)).mapTo[ActorRef]
        response <- (dest ? Insulted(insult)).mapTo[Retort]
      } {
        log.info("{} has sent a retort from actor {}", tank, dest)
        Main.commandStreamActor ! (tank, response)

        if (response.retort == InsultsActor.retortsMap(insult)) {
          log.info("The retort was right! The insulter takes the hit! Insulter is {}", s)
          Main.gameActor.tell(TakeHit, s)
        } else {
          log.info("The retort was wrong! The target takes the hit!")
          Main.gameActor.tell(TakeHit, dest)
        }

      }

  }

}
