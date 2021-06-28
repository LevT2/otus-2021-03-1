package me.chuwy.otusfp

import scala.util.Try

import cats.syntax.either._

sealed trait Command extends Product with Serializable

object Command {
  case object Echo extends Command
  case object Exit extends Command
  case class RunFiber(durationSec: Int) extends Command
  case class AddNumber(num: Int) extends Command
  case object ReadNumber extends Command
  case object SetDeferred extends Command

  def parse(s: String): Either[String, Command] =
    s.toLowerCase match {
      case "echo" => Echo.asRight
      case "exit" => Exit.asRight
      case "set-deferred" => SetDeferred.asRight
      case "read-number" => ReadNumber.asRight
      case cmd =>

        cmd.split(" ").toList match {
          case List("run-fiber", IntString(durationSec)) =>
            RunFiber(durationSec).asRight
          case List("add-number", IntString(num)) =>
            AddNumber(num).asRight
          case _ =>
            s"Command $s could not be recognized".asLeft
        }
    }

  private object IntString {
    def unapply(s: String): Option[Int] =
      Try(s.toInt).toOption
  }
}
