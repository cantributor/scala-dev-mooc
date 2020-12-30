package module1

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

      def orElse[B >: A](other: Option[B]): Option[B] = this match {
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

   sealed trait List[+A]{
     def ::[AA >: A](head: AA): List[AA] = Cons(head, this)

    def mkString: String = mkString(", ")

    def mkString(sep: String): String = {
       import List._

      def loop(l: List[A], acc: StringBuilder): StringBuilder = {
        l match {
         case List.Nil => acc
         case h :: Nil => acc.append(s"$h")
         case h :: t => loop(t, acc.append(s"$h$sep"))
        }
       }
      loop(this, new StringBuilder()).toString()
     }

     def map[B](f: A => B): List[B] = ???
   }

   object List{
    case object Nil extends List[Nothing]
    case class ::[A](head: A, tail: List[A]) extends List[A]
    val Cons = ::

    def apply[T](arg: T*): List[T] = {
     var l: List[T] = List.Nil
     arg.foreach(el => l = el :: l)
     l
    }
   }

   val list = 1 :: List.Nil

   /**
    *
    * Реализовать метод конс :: который позволит добавлять элемент в голову списка
    */


   /**
    *
    * Реализовать конструктор, для создания списка n элементов
    */


   /**
    *
    * Реализовать метод mkString который позволит красиво представить список в виде строки
    */


   /**
    *
    * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
    */


   /**
    *
    * Написать функцию incList котрая будет принимать список Int и возвращать список,
    * где каждый элемент будет увеличен на 1
    */


   /**
    *
    * Написать функцию shoutString котрая будет принимать список String и возвращать список,
    * где к каждому элементу будет добавлен префикс в виде '!'
    */


   /**
    *
    * Реализовать метод для списка который будет применять некую ф-цию к элементам данного списка
    */



 }