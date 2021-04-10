import cats._
import cats.implicits._

//import cats.instances.int._
//import cats.syntax.eq._

import java.util.Date
import cats.instances.function._ // for Functor
import cats.syntax.functor._ // for map
object Starter extends App {


  val eqInt = Eq[Int]
  println(eqInt.eqv(11, 11))
  println(1 eqv 2)
  println(1 === 2)
  println(1 =!= 2)

  println(1.some === 1.some)
  println(2.some === none[Int])

  val list1 = Functor[List].map(List(1, 2, 3))(identity)
  println(Functor[List].as(list1, "As"))

//  val func1 = (a: Int) => a + 1
//  val func2 = (a: Int) => a * 2
//  val func3 = (a: Int) => s"${a}!"
//  val func4 = func1.map(func2).map(func3)
//
//  func4(123)

}
