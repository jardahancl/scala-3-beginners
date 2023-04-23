package com.rockthejvm.part1basics

import scala.annotation.tailrec

object Recursion {

  // "repetition" = recursion
  def sumUntil(n: Int): Int =
    if (n <= 0) 0
    else n + sumUntil(n - 1) // "stack" recursion

  def sumUntil_v2(n: Int): Int = {
    /*
      sut(10, 0) =
      sut(9, 10) =
      sut(8, 9 + 10) =
      sut(7, 8 + 9 + 10) =
      ...
      sut(0, 1 + 2 + 3 + .. + 9 + 10)
      = 1 + 2 + 3 + .. + 10
     */
    @tailrec
    def sumUntilTailrec(x: Int, accumulator: Int): Int =
      if (x <= 0) accumulator
      else sumUntilTailrec(x - 1, accumulator + x) // TAIL recursion = recursive call occurs LAST in its code path
      // no further stack frames necessary = no more risk of SO

    sumUntilTailrec(n, 0)
  }

  def sumNumbersBetween(a: Int, b: Int): Int =
    if (a > b) 0
    else a + sumNumbersBetween(a + 1, b)

  def sumNumbersBetween_v2(a: Int, b: Int): Int = {
    @tailrec
    def sumTailrec(currentNumber: Int, accumulator: Int): Int =
      if (currentNumber > b) accumulator
      else sumTailrec(currentNumber + 1, currentNumber + accumulator)

    sumTailrec(a, 0)
  }

  /**
   * Exercises
   * 1. Concatenate a string n times
   * 2. Fibonacci function, tail recursive
   * 3. Is isPrime function tail recursive or not?
   */

  // 1
  def jConcatenate(s:String, n: Int): String = {
    @tailrec
    def concatTR(multiplicity: Int, result: String): String = {
      if (multiplicity == 0) result
      else concatTR(multiplicity - 1, result + s)
    }

    concatTR(n, "")
  }
  def concatenate(string: String, n: Int): String = {
    @tailrec
    def concatTailrec(remainingTimes: Int, accumulator: String): String =
      if (remainingTimes <= 0) accumulator
      else concatTailrec(remainingTimes - 1, string + accumulator)

    concatTailrec(n, "")
  }

  // 2
  def jFibonacci(n: Int): Int = {
    @tailrec
    def fibTR(step: Int, prev: Int, pprev: Int): Int = {
      if (step == 2) prev + pprev
      else fibTR(step - 1, prev + pprev, prev)
    }

    if (n == 0) 0
    else if (n <= 2) 1
    else fibTR(n-1, 1, 1)
  }
  def fibonacci(n: Int): Int = {
    def fiboTailrec(i: Int, last: Int, previous: Int): Int =
      if (i >= n) last
      else fiboTailrec(i + 1, last + previous, last)

    if (n <= 2) 1
    else fiboTailrec(2, 1, 1)
  }

  // 3 - yes, rephrasing:
  def jPrimeTest(n: Int): Boolean = {
    @tailrec
    def jPrimeTestWithLimit(n: Int, limit: Int): Boolean =
      if (n == 1) false
      else if (limit * limit > n) true
      else if (n % limit == 0) false
      else jPrimeTestWithLimit(n, limit + 1)

    jPrimeTestWithLimit(n, 2)
  }
  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean =
      if (t <= 1) true
      else if (n % t == 0) false
      else isPrimeUntil(t - 1)

    isPrimeUntil(n / 2)
  }

  def main(args: Array[String]): Unit = {
//    println(sumUntil_v2(20000))
//    println(concatenate("Scala", 5))
//    println(fibonacci(7))

//    println(sumUntil_v2(20000))
    println(jConcatenate("Scala", 5))
    println(jFibonacci(7))
  }
}
