package monads

import cats._
import cats.implicits._
import cats.syntax._
import cats.data.State

object StateTest extends App {

  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  // Get the state and the result:
  val (s, r) = a.run(10).value
  // state: Int = 10
  // result: String = "The state is 10"

  // Get the state, ignore the result:
  val justTheState = a.runS(10).value
  // justTheState: Int = 10

  // Get the result, ignore the state:
  val justTheResult = a.runA(10).value
  // justTheResult: String = "The state is 10"

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state1, result1) = step1.flatMap(s1 => step2.map(s2 => (s1, s2))).run(2).value

  val (state, result) = both.run(20).value
  println()
  // state: Int = 42
  // result: (String, String) = ("Result of step1: 21", "Result of step2: 42")
}
