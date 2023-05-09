package com.rockthejvm.jPractice

import scala.annotation.tailrec

// singly linked list
// [1,2,3] = [1] -> [2] -> [3] -> []
abstract class LList[A] {
  def head: A
  def tail: LList[A]
  def isEmpty: Boolean
  def add(element: A): LList[A] = Cons(element, this)

  def map[B](transformer: A => B): LList[B]
  def filter(predicate: A => Boolean): LList[A]
  def flatMap[B](transformer: A => LList[B]): LList[B]
  // concatenate LLists
  infix def ++(anotherList: LList[A]): LList[A]

  def foreach(f: A => Unit): Unit
  def sort(f: (A, A) => Int): LList[A]
  def zipWith[B, C](anotherList: LList[B], operation: (A, B) => C): LList[C]
  def folderLeft[B](start: B)(operation: (A, B) => B): B
}


case class Empty[A]() extends LList[A] {
  override def head = throw new NoSuchElementException
  override def tail = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  override def map[B](transformer: A => B): LList[B] = Empty()
  override def filter(predicate: A => Boolean): LList[A] = Empty()
  override def flatMap[B](transformer: A => LList[B]): LList[B] = Empty()
  override infix def ++(anotherList: LList[A]): LList[A] = anotherList

  override def foreach(f: A => Unit): Unit = {}
  override def sort(f: (A, A) => Int): LList[A] = Empty()
  override def zipWith[B, C](anotherList: LList[B], operation: (A, B) => C): LList[C] = {
    if (!anotherList.isEmpty) throw new IllegalArgumentException("Empty list zipping with non-empty one")
    else Empty()
  }
  override def folderLeft[B](start: B)(operation: (A, B) => B): B = start
}

case class Cons[A](override val head: A, override val tail: LList[A]) extends LList[A] {
  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def concatenateElements(remainder: LList[A], acc: String): String = {
      if (remainder.isEmpty)  acc + "]"
      else concatenateElements(remainder.tail, acc + ", " + remainder.head)
    }
    concatenateElements(this.tail, "[" + this.head)
  }

  override def map[B](transformer: A => B): LList[B] = {
    Cons(transformer(this.head), this.tail.map(transformer))
  }

  override def filter(predicate: A => Boolean): LList[A] = {
    if (predicate(this.head)) Cons[A](this.head, this.tail.filter(predicate))
    else this.tail.filter(predicate)
  }

  override def flatMap[B](transformer: A => LList[B]): LList[B] = {
    transformer(this.head) ++ this.tail.flatMap[B](transformer)
  }

  override infix def ++(anotherList: LList[A]): LList[A] = {
    Cons(this.head, this.tail ++ anotherList)
  }

  override def foreach(f: A => Unit): Unit = {
    f(this.head)
    this.tail.foreach(f)
  }

  override def sort(f: (A, A) => Int): LList[A] = {
    def insert(element: A, list: LList[A]): LList[A] = {
      if (list.isEmpty) Cons(element, Empty())
      else if (f(element, list.head) <= 0) Cons(element, list)
      else Cons(list.head, insert(element, list.tail))
    }

    val sortedTail = tail.sort(f)
    insert(this.head, sortedTail)
  }

  override def zipWith[B, C](anotherList: LList[B], operation: (A, B) => C): LList[C] = {
    if (anotherList.isEmpty) throw new IllegalArgumentException("Zipping lists of non-equal length")
    else Cons(operation(this.head, anotherList.head),
      this.tail.zipWith[B, C](anotherList.tail, operation))
  }

  override def folderLeft[B](start: B)(operation: (A, B) => B): B = {
    this.tail.folderLeft(operation(this.head, start))(operation)
  }

}

object LListTest {
  def main(args: Array[String]): Unit = {
    // empty
    val empty = Empty()
    println(empty)

    // cons
    val first3numbers = Cons(1, Cons(2, Cons(3, Empty())))
    println(first3numbers)

    // map
    val doubler = new ((Int) => Int) {
      override def apply(input: Int): Int = input * 2
    }
    val first3doubled = first3numbers.map(doubler)
    println(first3doubled)

    // filter
    val evenPredicate = new ((Int) => Boolean) {
      override def apply (input: Int): Boolean = (input % 2 == 0)
    }
    println(first3numbers.filter(evenPredicate))

    // flatMap
    val doublerList = new ((Int) => LList[Int]) {
      override def apply(input: Int): LList[Int] = Cons(input, Cons(2 * input, Empty()))
    }
    val first3doubledList = first3numbers.flatMap(doublerList)
    println(first3doubledList)

    //    val evenPredicate_v2 = new EvenPredicate
    //    val doubler_v2 = new Doubler
    //    val doublerList_v2 = new DoublerList
    //    println(first3numbers.map(doubler_v2))
    //    println(first3numbers.filter(evenPredicate_v2))
    //    println(first3numbers.flatMap(doublerList_v2))

    // foreach
    first3numbers.foreach(x => print(x + " "))
    println()

    // sort
    println(first3doubledList.sort((x, y) => x.compare(y)))

    // zipWith
    val loveScala = Cons("I ", Cons("love ", Cons("Scala.", Empty())))
    println(first3numbers.zipWith(first3doubled, (x, y) => x * y))
    println(loveScala.zipWith(first3numbers, (s, x) => x + "-" + s))

    // folderLeft
    println(first3doubledList.folderLeft[Int](15)((x, y) => x + y))
    println(loveScala.folderLeft[String]("Start: ")((s1, s2) => s2 + s1))

    // for-comprehension
    for {
      s <- loveScala
    } println(s)

  }
}

// classes below are not used
trait Predicate[T] {
  def test(input: T): Boolean
}

class EvenPredicate extends Predicate[Int] {
  override def test(input: Int): Boolean = (input % 2 == 0)
}

trait Transformer[A, B] {
  def transform(input: A): B
}

class Doubler extends Transformer[Int, Int] {
  override def transform(input: Int): Int = 2 * input
}

class DoublerList extends Transformer[Int, LList[Int]] {
  override def transform(input: Int): LList[Int] =
    Cons(input, Cons(input + 1, Empty()))
}

