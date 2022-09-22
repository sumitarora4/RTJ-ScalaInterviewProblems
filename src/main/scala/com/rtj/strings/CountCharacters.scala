package com.rtj.strings

import scala.annotation.tailrec

object CountCharacters {

  def main(args: Array[String]):Unit = {

    def countTheCharacters(str: String) = {

      // 1) normal iteration through mapValues function
      val result = str.groupBy(identity).mapValues(_.size).toMap
      println("result= "+result)

      // mapValues would not be depreciated with view
      val result2 = str.groupBy(identity).view.mapValues(_.size).toMap
      println("result2="+result2)

      //2) foreach iteration
      str.distinct.foreach( c => println(s" $c ->"+str.count (_== c)))

      //3) Through Tail Recursion
      @tailrec
      def countTheCharactersTailRec(remaining: String, acc: Map[Char, Int]):Map[Char, Int] ={
        if (remaining.isEmpty) acc
        else if(acc.contains(remaining.head)) {
          val currentOccurance = acc(remaining.head)
          countTheCharactersTailRec(remaining.tail, acc + (remaining.head -> (currentOccurance +1)))
        }
        else
          countTheCharactersTailRec(remaining.tail, acc + (remaining.head -> 1))
      }

      countTheCharactersTailRec(str, Map())
    }

    println(countTheCharacters("scala"))
    println(countTheCharacters("sumit kumar arora"))
  }
}
