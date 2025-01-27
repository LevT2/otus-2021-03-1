package me.chuwy.otusfp

import scala.concurrent.duration._

import cats.implicits._

import cats.effect.kernel.Resource
import cats.effect.{Ref, IOApp, IO, Deferred}

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
