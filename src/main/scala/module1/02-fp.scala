package module1

import module1.list.List
import module1.list.List.Cons

import scala.annotation.tailrec

/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   sealed trait Option[+A]{
      def isEmpty: Boolean = this match {
       case Option.Some(_) => false
       case Option.None => true
      }

      def get: A = this match {
       case Option.Some(v) => v
       case Option.None => throw new Exception("get on empty Option")
      }

      def printIfAny(): Unit = this match {
        case Option.Some(v) => println(v)
        case Option.None =>
      }

      def orElse[B >: A](other: => Option[B]): Option[B] = this match {
        case Option.Some(_) => this
        case Option.None => other
      }

      def zip[B >: A](other: Option[B]): Option[(A, B)] = (this, other) match {
        case (Option.Some(a), Option.Some(b)) => Option.Some((a, b))
        case _ => Option.None
      }

      def filter(pred: A => Boolean): Option[A] = this match {
        case Option.Some(v) if pred(v) => this
        case _ => Option.None
      }
  }

   object Option {
     case class Some[A](v: A) extends Option[A]
     case object None extends Option[Nothing]
   }
 }

 object recursion {

   /**
    * Реализовать метод вычисления n!
    * n! = 1 * 2 * ... n
    */

   def fact(n: Int): Long = {
    var _n = 1L
    var i = 2
    while (i <= n) {
     _n *= i
     i += 1
    }
    _n
   }

   def !!(n: Int): Long = {
     if(n <= 1) 1
     else n * !!(n - 1)
   }

  def !(n: Int): Long = {
   @tailrec
   def loop(n1: Int, acc: Long): Long = {
     if(n <= 1) acc
     else loop(n1 - 1, n1 * acc)
    }
   loop(n, 1)
  }

 }

object list {

  /**
   *
   * Реализовать односвязанный имутабельный список List
   */

  sealed trait List[+A] {
    import List._

    def ::[AA >: A](head: AA): List[AA] = Cons(head, this)

    def mkString: String = mkString(", ")
    def mkString(sep: String): String = {
      @tailrec
      def loop(l: List[A], acc: StringBuilder): StringBuilder = {
        l match {
          case List.Nil => acc
          case h :: List.Nil => acc.append(s"$h")
          case h :: t => loop(t, acc.append(s"$h$sep"))
        }
      }
      loop(this, new StringBuilder()).toString()
    }

    /**
     * Реверс с перевоплощением как основа для обычного реверса и мэппинга
     */
    private def reverse[B](f: A => B): List[B] = {
      @tailrec
      def loop(remainder: List[A], acc: List[B]): List[B] = {
        remainder match {
          case List.Nil => acc
          case head :: tail => loop(tail, f(head) :: acc)
        }
      }
      loop(this, List())
    }

    /**
     * Нормальный реверс как частный случай реверса с перевоплощением
     */
    def reverse: List[A] = reverse(identity)

    /**
     * Мэппинг как сочетание реверса с перевоплощением и простого реверса обратно
     */
    def map[B](f: A => B): List[B] = this.reverse(f).reverse
  }

  object List {

    case object Nil extends List[Nothing]

    case class ::[A](head: A, tail: List[A]) extends List[A]

    val Cons: ::.type = ::

    def apply[T](arg: T*): List[T] = {
      var l: List[T] = List.Nil
      arg.foreach(el => l = el :: l)
      l
    }
  }

  def incList(list: List[Int]): List[Int] = list.map(_ + 1)

  def shoutString(list: List[String]): List[String] = list.map("!" + _)

  val listInt: List[Int] = 1 :: 2 :: 3 :: List.Nil
  val listStr: List[String] = "Hello" :: "Goodbye" :: "See ya" :: List.Nil
}