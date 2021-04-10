package monads

import cats.Eval
import cats.implicits._
import cats.syntax._

object EvalTest extends App {

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      //      factorial(n - 1).map(_ * n)
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  //  println(factorial(50000).value)

  def foldRightEval[A, B](as: List[A], acc: Eval[B])
                         (fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  val bigFold = foldRight((1 to 100000).toList, 0L)(_ + _)
  println(bigFold)
}
