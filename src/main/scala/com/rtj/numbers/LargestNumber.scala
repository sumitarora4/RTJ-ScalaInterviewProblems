package com.rtj.numbers

object LargestNumber {

  def LargestNumberProblem(numbers: List[Int]): String = {

    implicit val numbersOrdering: Ordering[Int] = Ordering.fromLessThan { (a, b) =>

      val aString = a.toString
      val bString = b.toString

      (aString + bString).compareTo(bString + aString) >= 0


    }
    val largest = numbers.sorted.mkString("")

    if (numbers.isEmpty || largest.charAt(0) == '0') "0"
    else largest
  }

  def main(args: Array[String]): Unit = {

    println(LargestNumberProblem(List(2, 10)))
    println(LargestNumberProblem(List(2, 35, 47)))
    println(LargestNumberProblem(List(20,403)))
    println(LargestNumberProblem(List(20,0)))
  }

}
