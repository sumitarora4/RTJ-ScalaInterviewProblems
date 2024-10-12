package com.rtj.numbers

import scala.annotation.tailrec

object PrimeNumber extends  App{

  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeTailRec(currentDivisor: Int) :Boolean = {
      if (currentDivisor > Math.sqrt(Math.abs(n))) true //  square root of any positive number gives
      // all the possible divisors of number
      else n % currentDivisor != 0 && isPrimeTailRec(currentDivisor + 1)
    }

    if (n == 1 || n  ==0 || n== -1 ) false
    else isPrimeTailRec(2)
  }

  println(isPrime(2))
  println(isPrime(1))
  println(isPrime(22))
  println(isPrime(11))
  println(isPrime(2003))
}
