package com.rockthejvm.jPractice

import scala.annotation.tailrec

// singly linked list
// [1,2,3] = [1] -> [2] -> [3] -> []
abstract class LList[A] {
  def head: A
  def tail: LList[A]
  def isEmpty: Boolean
  def add(element: A): LList[A] = Cons(element, this)

  def map[B](transformer: Transformer[A, B]): LList[B]
  def filter(predicate: Predicate[A]): LList[A]
  def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B]

  infix def ++(anotherList: LList[A]): LList[A]
}


case class Empty[A]() extends LList[A] {
  override def head = throw new NoSuchElementException
  override def tail = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  override def map[B](transformer: Transformer[A, B]): LList[B] = Empty()
  override def filter(predicate: Predicate[A]): LList[A] = Empty()
  override def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B] = Empty()

  override infix def ++(anotherList: LList[A]): LList[A] = anotherList
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

  override def map[B](transformer: Transformer[A, B]): LList[B] = {
    Cons(transformer.transform(this.head), this.tail.map(transformer))
  }

  override def filter(predicate: Predicate[A]): LList[A] = {
    if (predicate.test(this.head)) Cons[A](this.head, this.tail.filter(predicate))
    else this.tail.filter(predicate)
  }

  override def flatMap[B](transformer: Transformer[A, LList[B]]): LList[B] = {
    transformer.transform(this.head) ++ this.tail.flatMap[B](transformer)
  }

  override infix def ++(anotherList: LList[A]): LList[A] = {
    Cons(this.head, this.tail ++ anotherList)
  }
}

object LListTest {
  def main(args: Array[String]): Unit = {
    val empty = Empty()
    println(empty)

    val first3numbers = Cons(1, Cons(2, Cons(3, Empty())))
    println(first3numbers)

    val doubler = new Transformer[Int, Int] {
      override def transform(input: Int): Int = input * 2
    }

    val evenPredicate = new Predicate[Int] {
      def test(input: Int): Boolean = (input % 2 == 0)
    }

    val doublerList = new Transformer[Int, LList[Int]] {
      override def transform(input: Int): LList[Int] = Cons(input, Cons(input, Empty()))
    }

    val evenPredicate_v2 = new EvenPredicate
    val doubler_v2 = new Doubler
    val doublerList_v2 = new DoublerList

    println(first3numbers.map(doubler))
    println(first3numbers.map(doubler_v2))

    println(first3numbers.filter(evenPredicate))
    println(first3numbers.filter(evenPredicate_v2))

    println(first3numbers.flatMap(doublerList))
    println(first3numbers.flatMap(doublerList_v2))

  }
}


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

