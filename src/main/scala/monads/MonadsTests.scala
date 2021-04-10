package monads

import cats.Id
import cats._
import cats.implicits._
import cats.syntax._


object MonadsTests extends App {

  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).flatMap { aNum =>
      parseInt(bStr).flatMap { bNum =>
        divide(aNum, bNum)
      }
    }

  val s1 = Some(1)
  val s2 = Some(2)
  val s3 = Some(3)
  s1.flatMap(a => s2.flatMap(b => s3.map(c => a + b + c)))

  val id = 10: Id[Int]
  println(id + 11)

  val asRight = 1.asRight[String]
  val asRight2 = 4.asRight
  val eitherRes = for {
    a <- asRight
    b <- asRight2
  } yield (a + b)
  println(eitherRes)

  val dd = Right.apply("asdf")

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }


  println(countPositive(List(1, 2, 3)))
  // res5: Either[String, Int] = Right(3)
  println(countPositive(List(1, -2, 3)))
  // res6: Either[String, Int] = Left("Negative. Stopping!")

  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchOnly[NumberFormatException]("33".toInt))

  println(Either.catchNonFatal(sys.error("Badness")))
  println(Either.catchNonFatal(1))
  // res8: Either[Throwable, Nothing] = Left(java.lang.RuntimeException: Badness)

  println(Either.fromOption[String, Int](None, "Badness"))
  println(Either.fromOption[String, Int](11.some, "Badness"))

  val erh = for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "DIV0".asLeft[Int]
    else (a / b).asRight[String]
  } yield c * 100

}
