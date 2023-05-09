package com.rockthejvm.jPractice

abstract class MaybeJ[A] {
  def map[B](operator: A => B): MaybeJ[B]
  def flatMap[B](operator: A => MaybeJ[B]): MaybeJ[B]
  def filter(pred: A => Boolean): MaybeJ[A]
}

case class MaybeJNot[A]() extends MaybeJ[A] {
  override def map[B](operator: A => B): MaybeJ[B] = MaybeJNot[B]()
  override def flatMap[B](operator: A => MaybeJ[B]): MaybeJ[B] = MaybeJNot[B]()
  override def filter(pred: A => Boolean): MaybeJ[A] = this
}

case class JustJ[A](val value: A) extends MaybeJ[A] {
  override def map[B](operator: A => B): MaybeJ[B] = JustJ(operator(value))
  override def flatMap[B](operator: A => MaybeJ[B]): MaybeJ[B] = operator(value)
  override def filter(pred: A => Boolean): MaybeJ[A] = {
    if (pred(value)) this
    else MaybeJNot[A]()
  }
}

object MaybeJTest {
  def main(args: Array[String]): Unit = {
    val maybeInt: MaybeJ[Int] = JustJ(3)
    val maybeInt2: MaybeJ[Int] = MaybeJNot()
    val maybeIncrementedInt = maybeInt.map(_ + 1)
    val maybeIncrementedInt2 = maybeInt2.map(_ + 1)
    println(maybeIncrementedInt)
    println(maybeIncrementedInt2)

    val maybeFiltered = maybeInt.filter(_ % 2 == 0)
    println(maybeFiltered)

    val maybeFlatMapped = maybeInt.flatMap(number => JustJ(number * 3))
    println(maybeFlatMapped)
  }
}
