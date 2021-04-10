package monads

import cats._
import cats.implicits._
import cats.syntax._

import scala.concurrent.{Await, Future}

object WriterTest extends App {

  import cats.data.Writer
  import cats.instances.vector._ // for Monoid

  val w1 = Writer(Vector(
    "It was the best of times",
    "it was the worst of times"
  ), 1859)

  println(w1.value)
  println(w1.written)
  val (w, r) = w1.run
  println(w)
  println(r)

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  //  factorial(5)

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )), 5.seconds)

  type Logged[A] = Writer[Vector[String], A]

  def factorial2(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial2(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  //  val (log, res) = factorial2(2).run
  //  println(log)
  //  println(res)
  val concurrentWriters = Await.result(Future.sequence(Vector(
    Future(factorial2(5)),
    Future(factorial2(5))
  )).map(_.map(_.written)), 5.seconds)
  println(concurrentWriters)

  println(Writer("asdfad", 1).written)
}
