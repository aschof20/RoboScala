package cosc250.roboScala

import java.awt.Color

/** Commands are sent by AI Players to the Game Actor */
sealed trait Command

/** Insults are forwarded to the InsultsActor for adjudication */
sealed trait InsultCommand extends Command

/** Register for the game */
case class Register(name:String, color:Color) extends Command

/** Rotate the turret */
case object TurretClockwise extends Command
case object TurretAnticlockwise extends Command

/** Fire a shell */
case object Fire extends Command

/** Rotate the tank body */
case object TurnClockwise extends Command
case object TurnAnticlockwise extends Command

/** Change the desired velocity for the tank */
case object FullSpeedAhead extends Command
case object FullReverse extends Command

/** Rotate the radar on top of the turret */
case object RadarClockwise extends Command
case object RadarAnticlockwise extends Command

/** Use the radar to scan for tanks in range */
case object RadarPing extends Command

/** A special command, added by the GameActor, to say that a tank has been hit */
case object TakeHit extends Command

/** Launches an devastating insult at a named tank */
case class Insult(tank:String, insult:String) extends InsultCommand

/** Asks the InsultsActor for the appropriate retort */
case class WhatsTheRetortFor(insult:String) extends InsultCommand

/** Responds with a devastating retort */
case class Retort(retort:String) extends InsultCommand