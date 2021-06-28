/*
 * Copyright (c) 2012-2020 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package me.chuwy.otusfp

import scala.concurrent.Future
import scala.concurrent.duration._

import org.specs2.mutable.Specification
import cats.effect.testing.specs2.CatsEffect
import cats.implicits._

import cats.effect.{IO, Async, Sync}
import cats.effect.implicits._
import cats.effect.kernel.Concurrent

class CancelSpec extends Specification with CatsEffect {
  "Cancel" should {
    "provide forceR" in {
      IO.raiseError(new RuntimeException("boom")).forceR(IO(5)).map { result =>
        result must beEqualTo(5)
      }
    }

//    "provide cancellable" in {
//      val fail = IO.sleep(100.millis) *> IO.raiseError(new RuntimeException("boom")).void
//      val noFail = (IO.sleep(1.second) *> IO.println("After 1 second")).uncancelable
//      // IO[A] => IO[Either[Throwable, A]]
//      IO.race(fail, noFail).attempt.map { result =>
//        result must beRight
//      }
//    }

    "provide onCancel" in {
      val fail = (IO.sleep(100.millis) *> IO.raiseError(new RuntimeException("boom")).void)
      val noFail = (IO.sleep(1.second) *> IO.println("After 1 second")).onCancel(IO.println("fail has failed"))

      IO.race(fail, noFail).attempt.map { result =>
        result must beRight
      }
    }
  }

  "Sync" should {
    "suspend" in {
      val a: IO[Unit] = IO {
        println("Hello")
        println("World")
      }


      import cats.effect.Spawn


      def getRequest[F[_]: Sync](url: String): F[Unit] = {
        Sync[F].blocking {
          println(s"Retriving ${url}")
        }
      }

      def exec[F[_]: Concurrent] =
        Concurrent[F].ref(3)


//      def async_[A](k: (Either[Throwable, A] => Unit) => Unit): F[A] =
//        async[A](cb => as(delay(k(cb)), None))

      def myCallback(result: Either[Throwable, Int]): Unit =
        result match {
          case Left(_) => println("Failure")
          case Right(_) => println("Success!")
        }

      import scala.concurrent.ExecutionContext.Implicits.global

      def asyncCreate[F[_]: Async](f: Future[String]) =
        Async[F].async_[String] { cb =>
          f.onComplete { tr =>
            cb(tr.toEither)

          }
        }

      ko
    }
  }
}
