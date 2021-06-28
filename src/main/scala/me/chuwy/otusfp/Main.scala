package me.chuwy.otusfp

import cats.data.State
import cats.effect.kernel.Resource
import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.implicits._

object World {
  case class RealWorld(events: List[String])

  val initial = RealWorld(List.empty)
  type WorldState[A] = State[RealWorld, A]
}

trait Cmd[F[_]] {
  def echo: F[Unit]

  def exit: F[Unit]

  def runFiber(durationSec: Int): F[Unit]

  def addNumber(num: Int): F[Unit]

  def readNumber(): F[Unit]

  def setDeferred: F[Unit]
}


object Cmd {

  import World._
  import Main.Environment

  def apply[F[_]](implicit ev: Cmd[F]): Cmd[F] = ev

  implicit val testInterpreter: Cmd[WorldState] = new Cmd[WorldState] {
    override def echo: WorldState[Unit] = ???

    override def exit: WorldState[Unit] = ???

    override def runFiber(durationSec: Int): WorldState[Unit] = ???

    override def addNumber(num: Int): WorldState[Unit] = ???

    override def readNumber(): WorldState[Unit] = ???

    override def setDeferred: WorldState[Unit] = ???
  }


  implicit val ioInterpreter: Cmd[IO] = new Cmd[IO] {

    val env = new Environment   // -- заглушка чтобы остальное не краснило

    override def echo: IO[Unit] = IO.readLine.flatMap(IO.println).as(true)

    override def exit: IO[Unit] = IO.pure(false)

    override def runFiber(durationSec: Int): IO[Unit] = {
      val print = env.ref.updateAndGet(_ + 1).flatMap(IO.println)
      val sleep = env.promise.get.as(true)
      val action = (sleep *> print).start.replicateA(10).void
      action *> IO.pure(true)
    }

    override def addNumber(num: Int): IO[Unit] =
      env.ref.update(start => start + num).as(true)

    override def readNumber(): IO[Unit] =
      env.ref.get.flatMap(IO.println).as(true)

    override def setDeferred: IO[Unit] =
      env.promise.complete(()).as(true)
  }
}


object Main extends IOApp.Simple {

  def process(env: Environment)(cmd: Command): IO[Boolean] =
    cmd match {
      case Command.Echo =>
        IO.readLine.flatMap(IO.println).as(true)
      case Command.Exit =>
        IO.pure(false)
      case Command.AddNumber(num) =>
        env.ref.update(start => start + num).as(true)
      case Command.ReadNumber =>
        env.ref.get.flatMap(IO.println).as(true)
      case Command.SetDeferred =>
        env.promise.complete(()).as(true)
      case Command.RunFiber(_) =>
        val print = env.ref.updateAndGet(_ + 1).flatMap(IO.println)
        val sleep = env.promise.get.as(true)
        val action = (sleep *> print).start.replicateA(10).void
        action *> IO.pure(true)
    }

  def program(env: Environment): IO[Unit] =
    IO.readLine.map(Command.parse).flatMap {
      case Right(cmd) => process(env)(cmd).flatMap {
        case true => program(env)
        case false => IO.println("Bye bye")
      }
      case Left(error) => IO.println(error)
    }

  case class Environment(ref: Ref[IO, Int], promise: Deferred[IO, Unit])

  object Environment {
    def build: Resource[IO, Environment] = {
      val deferred = Resource.make(Deferred.apply[IO, Unit]) { d =>
        d.tryGet.flatMap {
          case Some(_) => IO.println("Releasing Deferred after it has was completed")
          case None =>  IO.println("Releasing fresh Deferred ")
        }
      }

      for {
        ref <- Resource.make(Ref.of[IO, Int](0)) { _ => IO.println("Releasing Ref") }
        promise <- deferred
      } yield Environment(ref, promise)
    }
  }

  def run: IO[Unit] =
    Environment.build.use(program)
}
